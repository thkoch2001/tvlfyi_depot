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
    #local FIRST=$(git merge-base --fork-point HEAD origin/canon)
    local FIRST=$(git rev-parse HEAD~1)
    local SECOND=$(git rev-parse "$FIRST~1")
    local THIRD=$(git rev-parse "$FIRST~2")

    curl 'https://graphql.buildkite.com/v1' \
         --silent \
         -H "Authorization: Bearer $(cat /run/agenix/buildkite-graphql-token)" \
         -d "{\"query\": \"query { pipeline(slug: \\\"tvl/depot\\\") { builds(commit: [\\\"$FIRST\\\",\\\"$SECOND\\\",\\\"$THIRD\\\"]) { edges { node { uuid }}}}}\"}" | \
         jq -r '.data.pipeline.builds.edges[] | .node.uuid'
}

for build in $(most_relevant_builds); do
    echo "Checking artifacts for build $build"
    buildkite-agent artifact download --build "${build}" 'pipeline/drvmap.json' '.'

    if [[ -f "pipeline/drvmap.json" ]]; then
        echo "Fetched target map from build ${build}"
        mv pipeline/drvmap.json parent-target-map.json
        break
    fi
done
