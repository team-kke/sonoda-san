import axios from 'axios';
import env from './env';
import { download } from './util';
import {
  Client,
  ImageMessage,
  TextEventMessage,
} from '@line/bot-sdk';

const client = new Client(env);

export default async function handleTextMessage(
  message: TextEventMessage,
  replyToken: string
) {
  const text = message.text;

  if (!text.match(/^(園田|海未|そのだ|うみ)/)) {
    return;
  }

  const start = /[a-zA-Z0-9!@#\$%\^\&*\)\(+=._-]/.exec(text);

  if (!start) {
    return;
  }

  const input = text.slice(start.index);

  const res = await axios.get(
    'http://api.wolframalpha.com/v2/query',
    {
      params: { input, appid: env.wolframAppId },
    }
  );

  let images: string[] = [];

  let m;
  const imgExp = /<img src='(.+?)'/g;
  while (m = imgExp.exec(res.data)) {
    images.push(m[1].replace('&amp;', '&').replace('gif', 'jpeg'));
  }

  if (images.length === 0) {
    return;
  }

  // replyMessage can only send up to 5 messages
  images = images.slice(0, 5);

  const urls: string[] = [];

  await Promise.all(
    images.map(async (url, idx) => {
      const path = `downloaded/${message.id}_${idx}.jpeg`;
      await download(url, path);
      urls.push(`${env.baseURL}/${path}`);
    })
  );

  client.replyMessage(
    replyToken,
    urls.map((url) => ({
      type: 'image',
      originalContentUrl: url,
      previewImageUrl: url,
    }) as ImageMessage)
  );
}
