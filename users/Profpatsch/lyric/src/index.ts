import { tapBpm } from './tap-bpm.js';

function main() {
  // subcommand for tap-bpm
  if (process.argv[2] === 'tap-bpm') {
    tapBpm();
  }
}

main();

// sleep in a loop to block nodejs
console.log('Blocking event loop...');
while (true) {
  await new Promise(resolve => setTimeout(resolve, 1000));
}
