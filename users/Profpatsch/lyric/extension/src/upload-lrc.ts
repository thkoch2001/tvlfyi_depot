import * as crypto from 'crypto';

// Helper function to convert a hex string to a Buffer
function hexToBytes(hex: string): Buffer {
  return Buffer.from(hex, 'hex');
}

// Function to verify the nonce
function verifyNonce(result: Buffer, target: Buffer): boolean {
  if (result.length !== target.length) {
    return false;
  }

  for (let i = 0; i < result.length - 1; i++) {
    if (result[i] > target[i]) {
      return false;
    } else if (result[i] < target[i]) {
      break;
    }
  }

  return true;
}

// Function to solve the challenge
function solveChallenge(prefix: string, targetHex: string): string {
  let nonce = 0;
  let hashed: Buffer;
  const target = hexToBytes(targetHex);

  while (true) {
    const input = `${prefix}${nonce}`;
    hashed = crypto.createHash('sha256').update(input).digest();

    if (verifyNonce(hashed, target)) {
      break;
    } else {
      nonce += 1;
    }
  }

  return nonce.toString();
}

async function getUploadNonce() {
  try {
    const response = await fetch('https://lrclib.net/api/request-challenge', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'User-Agent': 'lyric tool (https://code.tvl.fyi/tree/users/Profpatsch/lyric)',
        'Lrclib-Client': 'lyric tool (https://code.tvl.fyi/tree/users/Profpatsch/lyric)',
      },
    });

    if (!response.ok) {
      throw new Error(`HTTP error! Status: ${response.status}`);
    }

    const challengeData = (await response.json()) as { prefix: string; target: string };

    return {
      prefix: challengeData.prefix,
      nonce: solveChallenge(challengeData.prefix, challengeData.target),
    };
  } catch (error) {
    console.error('Error fetching the challenge:', error);
  }
}

// Interface for the request body
/**
 * Represents a request to publish a track with its associated information.
 */
export interface PublishRequest {
  trackName: string;
  artistName: string;
  albumName: string;
  /** In seconds? Milliseconds? mm:ss? */
  duration: number;
  plainLyrics: string;
  syncedLyrics: string;
}

// Function to publish lyrics using the solved challenge
export async function publishLyrics(
  requestBody: PublishRequest,
): Promise<true | undefined> {
  const challenge = await getUploadNonce();
  if (!challenge) {
    return;
  }
  const publishToken = `${challenge.prefix}:${challenge.nonce}`;

  const response = await fetch('https://lrclib.net/api/publish', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'User-Agent': 'lyric tool (https://code.tvl.fyi/tree/users/Profpatsch/lyric)',
      'Lrclib-Client': 'lyric tool (https://code.tvl.fyi/tree/users/Profpatsch/lyric)',
      'X-Publish-Token': publishToken,
    },
    body: JSON.stringify(requestBody),
  });

  if (response.status === 201) {
    console.log('Lyrics successfully published.');
    return true;
  } else {
    const errorResponse = (await response.json()) as { [key: string]: string };
    console.error('Failed to publish lyrics:', errorResponse);
    return;
  }
}
