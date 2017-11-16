import { exec } from './util';

export async function convert(from: string, to: string) {
  return exec(`convert ${from} ${to}`);
}

export async function smush(images: string[], to: string) {
  const ls = images.join(' ');
  return exec(`convert ${ls} -smush 7 ${to}`);
}

export async function resize(from: string, size: string, to: string) {
  return exec(`convert -resize ${size} ${from} ${to}`);
}
