import * as express from 'express';
import env from './env';
import handleTextMessage from './handler';
import {
  WebhookEvent,
  middleware,
} from '@line/bot-sdk';

const app = express();

app.use('/downloaded', express.static('downloaded'));

app.post('/callback', middleware(env), (req, res) => {
  const events: WebhookEvent[] = req.body.events;

  for (const event of events) {
    if (event.type === 'message' && event.message.type === 'text') {
      handleTextMessage(event.message, event.replyToken);
    }
  }

  res.end();
});


const port = env.port || 3000;
app.listen(port, () => console.log(`listening ${port}`));
