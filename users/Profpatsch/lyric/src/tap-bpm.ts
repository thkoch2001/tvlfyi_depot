// create a node command line listener that allows the user to press any key , and averages the distances between the key presses to determine the BPM (with a window of 4 key presses). If the user presses q, the program should exit and print the final BPM.

// Import the necessary modules
import * as readline from "readline";

export function tapBpm() {
  // Set up readline interface to listen for keypresses
  readline.emitKeypressEvents(process.stdin);
  process.stdin.setRawMode(true);
  // accept SIGINT on stdin

  // Array to store the time differences between the last 4 key presses
  const timeDifferences: number[] = [];
  let lastPressTime: number | null = null;

  // Function to calculate BPM based on average time between keypresses
  function calculateBPM() {
    if (timeDifferences.length < 1) {
      return 0;
    }
    const averageTimeDiff =
      timeDifferences.reduce((acc, curr) => acc + curr, 0) /
      timeDifferences.length;
    return (60 * 1000) / averageTimeDiff;
  }

  // Handle the SIGINT (Ctrl+C) event manually
  process.on("SIGINT", () => {
    console.log(
      "\nExiting via SIGINT (Ctrl+C)... Final BPM:",
      calculateBPM().toFixed(2)
    );
    process.exit();
  });

  // Listen for keypress events
  process.stdin.on("keypress", (str, key) => {
    // Exit if 'q' is pressed
    if (key.name === "q") {
      console.log("Exiting... Final BPM:", calculateBPM().toFixed(2));
      process.exit();
    }

    // Handle Ctrl+C (SIGINT)
    if (key.sequence === "\u0003") {
      // '\u0003' is the raw code for Ctrl+C
      console.log(
        "\nExiting via Ctrl+C... Final BPM:",
        calculateBPM().toFixed(2)
      );
      process.exit();
    }

    // Capture the current time of the keypress
    const currentTime = Date.now();

    // If it's not the first keypress, calculate the time difference
    if (lastPressTime !== null) {
      const timeDiff = currentTime - lastPressTime;

      // Add the time difference to the array (limit to last 10 key presses)
      if (timeDifferences.length >= 10) {
        timeDifferences.shift(); // Remove the oldest time difference
      }
      timeDifferences.push(timeDiff);

      // Calculate and display the BPM
      const bpm = calculateBPM();
      console.log("Current BPM:", bpm.toFixed(2));
    } else {
      console.log("Waiting for more key presses to calculate BPM...");
    }

    // Update the lastPressTime to the current time
    lastPressTime = currentTime;
  });

  console.log('Press any key to calculate BPM, press "q" to quit.');
}
