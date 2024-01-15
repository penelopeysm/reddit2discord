## reddit2discord

Small script to act as a bridge between a Reddit post and a Discord channel.

To run:

1. Install [`cabal`, the Haskell build tool](https://www.haskell.org/cabal/). Figure this out yourself with the instructions there.
2. Edit the variables at the top level of the script. You need:
  - The post ID of the Reddit post. This should be 7 alphanumeric characters long, and can be found in the URL (`https://reddit.com/r/<subreddit>/comments/<postID>/...`).
  - The name of the subreddit that the post is on.
  - The channel ID of the Discord channel to post to. You need to enable developer mode on Discord, then right-click on the channel name and copy the ID.
3. Set up the following environment variables. You will need:
  - `DISCORD_TOKEN` Discord bot token.
  - `REDDIT_ORIG_USERNAME` Your Reddit username.
  - `REDDIT_ORIG_PASSWORD` Your Reddit password.
  - `REDDIT_ID` Reddit API client ID
  - `REDDIT_SECRET` Reddit API client secret
4. Run with `cabal run`

The script will:

- Post all new comments on the post into the Discord channel.
- If you reply in Discord to a comment, post a comment on Reddit replying to the same comment. The script will also upvote the comment you replied to (this is personal, if you don't like it, remove the lines with upvote).
- If you react with 👍 or 👎 it will upvote or downvote the comment accordingly. Reacting with ❌ will remove any existing vote.
