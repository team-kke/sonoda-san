import axios from 'axios';
import { createWriteStream } from 'fs';
import * as cp from 'child_process';

export async function download(from: string, to: string) {
  const { data } = await axios({
    method: 'get',
    url: from,
    responseType:'stream',
  });

  return new Promise((resolve, reject) => {
    const writable = createWriteStream(to);
    data.pipe(writable);
    writable.on('close', resolve);
    writable.on('error', reject);
  });
}

export function exec(command: string): Promise<string> {
  return new Promise((
    resolve: (result: string) => void,
    reject,
  ) => {
    cp.exec(command, (err, stdout) => {
      if (err) {
        reject(err);
      } else {
        resolve(stdout);
      }
    });
  });
}

export function rm(file: string) {
  return exec(`rm ${file}`);
}
