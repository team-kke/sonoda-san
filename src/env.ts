export default {
  baseURL: process.env.BASE_URL,
  channelSecret: process.env.CHANNEL_SECRET,
  channelAccessToken: process.env.CHANNEL_ACCESS_TOKEN,
  port: parseInt(process.env.PORT, 10),
  wolframAppId: process.env.WOLFRAM_APP_ID,
};
