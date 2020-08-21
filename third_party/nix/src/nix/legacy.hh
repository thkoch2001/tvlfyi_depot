#pragma once

#include <functional>
#include <map>
#include <string>

namespace nix {

typedef std::function<void(int, char**)> MainFunction;

struct RegisterLegacyCommand {
  using Commands = std::map<std::string, MainFunction>;
  static Commands* commands;

  RegisterLegacyCommand(const std::string& name, MainFunction fun) {
    if (!commands) {
      commands = new Commands;
    }
    (*commands)[name] = fun;
  }
};

}  // namespace nix
