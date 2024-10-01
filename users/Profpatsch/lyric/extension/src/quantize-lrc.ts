export function bpmToEighthNoteDuration(bpm: number): number {
  // Convert BPM to eighth-note duration in milliseconds
  const quarterNoteDuration = (60 / bpm) * 1000; // in ms
  const eighthNoteDuration = quarterNoteDuration / 2;
  return eighthNoteDuration;
}

function parseTimestamp(timestamp: string): number {
  // Parse [mm:ss.ms] format into milliseconds
  const [min, sec] = timestamp.split(':');

  const minutes = parseInt(min, 10);
  const seconds = parseFloat(sec);

  return minutes * 60 * 1000 + seconds * 1000;
}

function formatTimestamp(ms: number): string {
  // Format milliseconds back into [mm:ss.ms]
  const minutes = Math.floor(ms / 60000);
  ms %= 60000;
  const seconds = (ms / 1000).toFixed(2);

  return `${String(minutes).padStart(2, '0')}:${seconds}`;
}

export function adjustTimestampToEighthNote(
  timestampMs: number,
  eighthNoteDuration: number,
): number {
  // Find the closest multiple of the eighth-note duration
  return Math.round(timestampMs / eighthNoteDuration) * eighthNoteDuration;
}

function adjustTimestamps(bpm: number, timestamps: string[]): string[] {
  const eighthNoteDuration = bpmToEighthNoteDuration(bpm);

  return timestamps.map(timestamp => {
    const timestampMs = parseTimestamp(timestamp);
    const adjustedMs = adjustTimestampToEighthNote(timestampMs, eighthNoteDuration);
    return formatTimestamp(adjustedMs);
  });
}

// Parse a .lrc file into an array of objects with timestamp and text
// Then adjust the timestamps to the closest eighth note
// Finally, format the adjusted timestamps back into [mm:ss.ms] format and put them back into the lrc object
//
// Example .lrc file:
// [01:15.66] And the reviewers bewail
// [01:18.18] There'll be no encore
// [01:21.65] 'Cause you're not begging for more
// [01:25.00]
// [01:34.64] She may seem self-righteous and holier-than-thou
// [01:39.77] She may sound like she has all the answers
// [01:45.20] But beyond she may feel just a bit anyhow
function parseLrc(lrc: string): { timestamp: string; text: string }[] {
  return lrc
    .trimEnd()
    .split('\n')
    .map(line => {
      const match = line.match(/\[(\d+:\d+\.\d+)\](.*)/);
      const [, timestamp, text] = match!;
      return { timestamp, text };
    });
}

function formatLrc(lrc: { timestamp: string; text: string }[]): string {
  return lrc.map(({ timestamp, text }) => `[${timestamp}] ${text}`).join('\n');
}

function adjustLrc(lrc: string, bpm: number): string {
  const lrcArray = parseLrc(lrc);
  const timestamps = lrcArray.map(({ timestamp }) => timestamp);
  const adjustedTimestamps = adjustTimestamps(bpm, timestamps);

  lrcArray.forEach((line, i) => {
    line.timestamp = adjustedTimestamps[i];
  });

  return formatLrc(lrcArray);
}
