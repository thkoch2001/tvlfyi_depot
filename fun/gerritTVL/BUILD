load("//tools/bzl:plugin.bzl", "gerrit_plugin")

gerrit_plugin(
    name = "tvl",
    srcs = glob(["src/main/java/**/*.java"]),
    manifest_entries = [
        "Gerrit-PluginName: tvl",
        "Gerrit-HttpModule: fyi.tvl.code.gerrit.HttpModule",
    ],
)
