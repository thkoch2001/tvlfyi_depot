#include "profiles.hh"

#include <cerrno>
#include <cstdio>

#include <glog/logging.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "store-api.hh"
#include "util.hh"

namespace nix {

static bool cmpGensByNumber(const Generation& a, const Generation& b) {
  return a.number < b.number;
}

/* Parse a generation name of the format
   `<profilename>-<number>-link'. */
static int parseName(const std::string& profileName, const std::string& name) {
  if (std::string(name, 0, profileName.size() + 1) != profileName + "-") {
    return -1;
  }
  std::string s = std::string(name, profileName.size() + 1);
  std::string::size_type p = s.find("-link");
  if (p == std::string::npos) {
    return -1;
  }
  int n;
  if (string2Int(std::string(s, 0, p), n) && n >= 0) {
    return n;
  }
  return -1;
}

Generations findGenerations(const Path& profile, int& curGen) {
  Generations gens;

  Path profileDir = dirOf(profile);
  std::string profileName = baseNameOf(profile);

  for (auto& i : readDirectory(profileDir)) {
    int n;
    if ((n = parseName(profileName, i.name)) != -1) {
      Generation gen;
      gen.path = profileDir + "/" + i.name;
      gen.number = n;
      struct stat st;
      if (lstat(gen.path.c_str(), &st) != 0) {
        throw SysError(format("statting '%1%'") % gen.path);
      }
      gen.creationTime = st.st_mtime;
      gens.push_back(gen);
    }
  }

  gens.sort(cmpGensByNumber);

  curGen = pathExists(profile) ? parseName(profileName, readLink(profile)) : -1;

  return gens;
}

static void makeName(const Path& profile, unsigned int num, Path& outLink) {
  Path prefix = (format("%1%-%2%") % profile % num).str();
  outLink = prefix + "-link";
}

Path createGeneration(const ref<LocalFSStore>& store, const Path& profile,
                      const Path& outPath) {
  /* The new generation number should be higher than old the
     previous ones. */
  int dummy;
  Generations gens = findGenerations(profile, dummy);

  unsigned int num;
  if (!gens.empty()) {
    Generation last = gens.back();

    if (readLink(last.path) == outPath) {
      /* We only create a new generation symlink if it differs
         from the last one.

         This helps keeping gratuitous installs/rebuilds from piling
         up uncontrolled numbers of generations, cluttering up the
         UI like grub. */
      return last.path;
    }

    num = gens.back().number;
  } else {
    num = 0;
  }

  /* Create the new generation.  Note that addPermRoot() blocks if
     the garbage collector is running to prevent the stuff we've
     built from moving from the temporary roots (which the GC knows)
     to the permanent roots (of which the GC would have a stale
     view).  If we didn't do it this way, the GC might remove the
     user environment etc. we've just built. */
  Path generation;
  makeName(profile, num + 1, generation);
  store->addPermRoot(outPath, generation, false, true);

  return generation;
}

static void removeFile(const Path& path) {
  if (remove(path.c_str()) == -1) {
    throw SysError(format("cannot unlink '%1%'") % path);
  }
}

void deleteGeneration(const Path& profile, unsigned int gen) {
  Path generation;
  makeName(profile, gen, generation);
  removeFile(generation);
}

static void deleteGeneration2(const Path& profile, unsigned int gen,
                              bool dryRun) {
  if (dryRun) {
    LOG(INFO) << "would remove generation " << gen;
  } else {
    LOG(INFO) << "removing generation " << gen;
    deleteGeneration(profile, gen);
  }
}

void deleteGenerations(const Path& profile,
                       const std::set<unsigned int>& gensToDelete,
                       bool dryRun) {
  PathLocks lock;
  lockProfile(lock, profile);

  int curGen;
  Generations gens = findGenerations(profile, curGen);

  if (gensToDelete.find(curGen) != gensToDelete.end()) {
    throw Error(format("cannot delete current generation of profile %1%'") %
                profile);
  }

  for (auto& i : gens) {
    if (gensToDelete.find(i.number) == gensToDelete.end()) {
      continue;
    }
    deleteGeneration2(profile, i.number, dryRun);
  }
}

void deleteGenerationsGreaterThan(const Path& profile, int max, bool dryRun) {
  PathLocks lock;
  lockProfile(lock, profile);

  int curGen;
  bool fromCurGen = false;
  Generations gens = findGenerations(profile, curGen);
  for (auto i = gens.rbegin(); i != gens.rend(); ++i) {
    if (i->number == curGen) {
      fromCurGen = true;
      max--;
      continue;
    }
    if (fromCurGen) {
      if (max != 0) {
        max--;
        continue;
      }
      deleteGeneration2(profile, i->number, dryRun);
    }
  }
}

void deleteOldGenerations(const Path& profile, bool dryRun) {
  PathLocks lock;
  lockProfile(lock, profile);

  int curGen;
  Generations gens = findGenerations(profile, curGen);

  for (auto& i : gens) {
    if (i.number != curGen) {
      deleteGeneration2(profile, i.number, dryRun);
    }
  }
}

void deleteGenerationsOlderThan(const Path& profile, time_t t, bool dryRun) {
  PathLocks lock;
  lockProfile(lock, profile);

  int curGen;
  Generations gens = findGenerations(profile, curGen);

  bool canDelete = false;
  for (auto i = gens.rbegin(); i != gens.rend(); ++i) {
    if (canDelete) {
      assert(i->creationTime < t);
      if (i->number != curGen) {
        deleteGeneration2(profile, i->number, dryRun);
      }
    } else if (i->creationTime < t) {
      /* We may now start deleting generations, but we don't
         delete this generation yet, because this generation was
         still the one that was active at the requested point in
         time. */
      canDelete = true;
    }
  }
}

void deleteGenerationsOlderThan(const Path& profile,
                                const std::string& timeSpec, bool dryRun) {
  time_t curTime = time(nullptr);
  std::string strDays = std::string(timeSpec, 0, timeSpec.size() - 1);
  int days;

  if (!string2Int(strDays, days) || days < 1) {
    throw Error(format("invalid number of days specifier '%1%'") % timeSpec);
  }

  time_t oldTime = curTime - days * 24 * 3600;

  deleteGenerationsOlderThan(profile, oldTime, dryRun);
}

void switchLink(const Path& link, Path target) {
  /* Hacky. */
  if (dirOf(target) == dirOf(link)) {
    target = baseNameOf(target);
  }

  replaceSymlink(target, link);
}

void lockProfile(PathLocks& lock, const Path& profile) {
  lock.lockPaths({profile},
                 (format("waiting for lock on profile '%1%'") % profile).str());
  lock.setDeletion(true);
}

std::string optimisticLockProfile(const Path& profile) {
  return pathExists(profile) ? readLink(profile) : "";
}

}  // namespace nix
