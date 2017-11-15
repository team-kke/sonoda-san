import axios from 'axios';
import { createWriteStream } from 'fs';

export async function download(from: string, to: string) {
  const { data } = await axios({
    method: 'get',
    url: from,
    responseType:'stream',
  });

  return new Promise((resolve, reject) => {
    data.pipe(createWriteStream(to));
    data.on('end', resolve);
    data.on('error', reject);
  });
}
