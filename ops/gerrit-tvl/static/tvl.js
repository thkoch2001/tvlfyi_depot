// vim: set noai ts=2 sw=2 et: */

const BUILDKITE_TOKEN = 'a150658fb61062e432f13a032962d70fa9352088';

function encodeParams(p) {
  const pieces = [];
  for (let k of Object.getOwnPropertyNames(p)) {
    pieces.push(`${encodeURIComponent(k)}=${encodeURIComponent(p[k])}`);
  }
  return pieces.join('&');
}

function formatDuration(from, to) {
  const millisecondsTook = Math.floor(to.valueOf() - from.valueOf());
  if (millisecondsTook < 2000) return `${millisecondsTook} ms`;
  const secondsTook = Math.floor(millisecondsTook / 1000);
  if (secondsTook < 100) return `${secondsTook} seconds`;
  const minutesTook = Math.floor(secondsTook / 60);
  if (minutesTook < 60) return `${minutesTook} minutes`;
  const hoursTook = Math.floor(minutesTook / 60);
  const minutesRemainder = minutesTook - (hoursTook * 60);
  return `${hoursTook}hr ${minutesRemainder}min`;
}

const tvlChecksProvider = {
  async fetch(change) {
    let {changeNumber, patchsetNumber, repo} = change;

    const experiments = window.ENABLED_EXPERIMENTS || [];
    if (experiments.includes("UiFeature__tvl_check_debug")) {
      changeNumber = 2872;
      patchsetNumber = 4;
      repo = 'depot';
    }

    if (repo !== 'depot') {
      // We only handle TVL's depot at the moment.
      return {responseCode: 'OK'};
    }

    const params = {
      // besadii uses the patchset ref as the branch name.
      branch: `refs/changes/${changeNumber.toString().slice(-2)}/${changeNumber}/${patchsetNumber}`,
    };
    const url = `https://api.buildkite.com/v2/organizations/tvl/pipelines/depot/builds?${encodeParams(params)}`;
    const resp = await fetch(url, {
      headers: {
        Authorization: `Bearer ${BUILDKITE_TOKEN}`,
      },
    });
    const respJSON = await resp.json();

    const runs = [];
    for (let i = 0; i < respJSON.length; i++) {
      const attempt = respJSON.length - i;
      const build = respJSON[i];

      for (let job of build.jobs) {
        // TODO(lukegb): add the ability to retry these (sometimes whitby runs out of disk...)
        const checkRun = {
          attempt: attempt,
          externalId: job.id,
          checkName: job.name,
          checkDescription: job.command,
          checkLink: job.web_url,
          status: {
            'running': 'RUNNING',
            'scheduled': 'RUNNABLE',
            'passed': 'COMPLETED',
            'failed': 'COMPLETED',
            'blocked': 'RUNNABLE',
            'canceled': 'COMPLETED',
            'canceling': 'RUNNING',
            'skipped': 'COMPLETED',
            'not_run': 'COMPLETED',
          }[job.state],
          labelName: 'Verified',
        };
        if (job.scheduled_at) {
          checkRun.scheduledTimestamp = new Date(job.scheduled_at);
        }
        if (job.started_at) {
          checkRun.startedTimestamp = new Date(job.started_at);
        }
        if (job.finished_at) {
          checkRun.finishedTimestamp = new Date(job.finished_at);
        }

        let statusDescription = job.state;
        if (checkRun.startedTimestamp && checkRun.finishedTimestamp) {
          statusDescription = `${statusDescription} in ${formatDuration(checkRun.startedTimestamp, checkRun.finishedTimestamp)}`;
        } else if (checkRun.startedTimestamp) {
          statusDescription = `${statusDescription} for ${formatDuration(checkRun.startedTimestamp, new Date())}`;
        } else if (checkRun.scheduledTimestamp) {
          statusDescription = `${statusDescription} for ${formatDuration(checkRun.scheduledTimestamp, new Date())}`;
        }
        checkRun.statusDescription = statusDescription;

        if (job.state === 'failed') {
          const result = {
            // TODO(lukegb): get the log as the message here (the Gerrit
            // implementation doesn't yet seem to support newlines in message
            // strings...)
            links: [{
              url: job.web_url,
              tooltip: "Buildkite",
              primary: true,
              icon: 'EXTERNAL',
            }],
            category: 'ERROR',
            summary: `${job.command} failed`,
          };
          checkRun.results = [result];
        }

        console.log(checkRun);
        runs.push(checkRun);
      }
    }

    return {
      responseCode: 'OK',
      runs: runs,
    };
  },
};

Gerrit.install(plugin => {
  console.log('TVL plugin initialising');

  plugin.checks().register(tvlChecksProvider);
});
