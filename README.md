# 喋る園田さん

<img width='250' src='https://cloud.githubusercontent.com/assets/1013641/19834661/5c166810-9eac-11e6-9fa4-903697e6e14d.png'>

> 最低です

## What is this?

A [LINE](https://line.me/) bot of [Wolfram Alpha](https://www.wolframalpha.com), likely to have something to do with [Sonoda Umi](https://ja.wikipedia.org/wiki/%CE%9C%27s#.E5.9C.92.E7.94.B0_.E6.B5.B7.E6.9C.AA.EF.BC.88.E3.81.9D.E3.81.AE.E3.81.A0_.E3.81.86.E3.81.BF.EF.BC.89).

## How to use

Set env vars:

``` shell
export BASE_URL=https://your.domain.com
export CHANNEL_SECRET=YOUR_CHANNEL_SECRET
export CHANNEL_ACCESS_TOKEN=YOUR_ACCESS_TOKEN
export PORT=5050
export WOLFRAM_APP_ID=XXXXXX-XXXXXXXXXX
```

Build and run:

``` shell
npm run build
node dist
```

## License

[MIT](LICENSE)
