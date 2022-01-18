#!/usr/bin/env bash
set -ueo pipefail

# Each Buildkite build stores the derivation target map as a pipeline
# artifact. This script determines the most appropriate commit (the
# fork point of the current chain from canon) and fetches the
# artifact.
#
# Since builds can be based on canon before the pipeline for the last
# commit has finished, it is possible that the fork point has no
# target map. To account for this, we will go up to 3 commits back in
# time to find a map.
#
# If no map is found, the failure mode is not critical: We simply
# build all targets.

function most_relevant_builds {
    git fetch -v origin canon
    local FIRST=$(git merge-base --fork-point HEAD origin/canon)
    local SECOND=$(git rev-parse "$FIRST~1")
    local THIRD=$(git rev-parse "$FIRST~2")

    curl 'https://graphql.buildkite.com/v1' \
         --silent \
         -H "Authorization: Bearer $(cat /run/agenix/buildkite-graphql-token)" \
         -d "{\"query\": \"query { pipeline(slug: \\\"tvl/depot\\\") { builds(commit:[\\\"$FIRST\\\",\\\"$SECOND\\\",\\\"$THIRD\\\"],state:[PASSED, BLOCKED]) { edges { node { uuid }}}}}\"}" | \
         jq -r '.data.pipeline.builds.edges[] | .node.uuid'
}

mkdir -p tmp
for build in $(most_relevant_builds); do
    echo "Checking artifacts for build $build"
    buildkite-agent artifact download --build "${build}" 'pipeline/drvmap.json' 'tmp/' || true

    if [[ -f "tmp/pipeline/drvmap.json" ]]; then
        echo "Fetched target map from build ${build}"
        mv tmp/pipeline/drvmap.json parent-target-map.json
        break
    fi
done
