# delilah

**[statuspage.io](https://www.statuspage.io/) now supports Slack, so this is pretty much useless.**

A web service to post [statuspage.io](https://www.statuspage.io/) updates to Slack.

> :fire: Service update: `github` changed its status from *operational* to **major outage**

> :heavy_check_mark: Service update: `github` changed its status from *major outage* to **operational**

## Deploying to Heroku

[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy)

You'll have to add a `SLACK_URL` environment variable with a [Slack webhook URL](https://api.slack.com/messaging/webhooks).

Then, for each service you want to subscribe to, add a `SERVICE_TOKEN_<service name>` environment variable with a random token that will be used to authenticate requests to delilah.

### Example with GitHub

1. Add your `SLACK_URL` to the environment variables
2. Add a `SERVICE_TOKEN_GITHUB` environment variable with a random token
3. Deploy the app
4. Go to https://www.githubstatus.com/
5. Click "Subscribe" and select the `<>` tab
6. Fill in the URL. It should look something like this: `https://your-heroku-app.com/status/github?token=<your random token>`
7. Add your email, click "Subscribe to notifications"
8. You're done!
