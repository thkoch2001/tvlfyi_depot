import { tapBpm } from "./tap-bpm.js";

async function main() {
  // subcommand for tap-bpm
  if (process.argv[2] === "tap-bpm") {
    await tapBpm();
  }
}

await main();

// sleep in a loop to block nodejs
console.log("Blocking event loop...");
while (true) {
  await new Promise((resolve) => setTimeout(resolve, 1000));
}
