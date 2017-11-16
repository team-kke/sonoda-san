import axios from 'axios';
import env from './env';
import { download, rm } from './util';
import * as im from './imagemagick';
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
    images.push(m[1].replace('&amp;', '&'));
  }

  if (images.length === 0) {
    return;
  }

  images = await Promise.all(
    images.map(
      async (url, idx) => {
        const gif = `downloaded/${message.id}_${idx}.gif`;
        const jpeg = `downloaded/${message.id}_${idx}.jpeg`;
        await download(url, gif);
        await im.convert(gif, jpeg);
        await rm(gif);
        return jpeg;
      }
    )
  );

  const resultImagePath = `downloaded/${message.id}.jpeg`;
  const previewImagePath = `downloaded/${message.id}_preview.jpeg`;
  await im.smush(images, resultImagePath);
  await im.resize(resultImagePath, '240x', previewImagePath);
  await Promise.all(images.map(rm));

  client.replyMessage(
    replyToken,
    {
      type: 'image',
      originalContentUrl: `${env.baseURL}/${resultImagePath}`,
      previewImageUrl: `${env.baseURL}/${previewImagePath}`,
    }
  );
}
