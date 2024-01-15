{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (throwIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Reddit
import System.Environment (getEnv)

-- The post to monitor
postID :: ID Post
postID = PostID "196laf2"

-- The subreddit the post is on
subreddit :: Text
subreddit = "pokemontrades"

-- The Discord channel to post to
chanID :: ChannelId
chanID = DiscordId $ Snowflake 1196157634223542282

getEnvAsText :: String -> IO T.Text
getEnvAsText = fmap T.pack . getEnv

replyIfPostIdMatches :: Comment -> RedditT IO ()
replyIfPostIdMatches c = when (commentPostId c == postID) $ do
  liftIO $ print c
  void $ liftIO $ forkIO $ postToDiscord c

postToDiscord :: Comment -> IO ()
postToDiscord c = do
  discordToken <- liftIO $ getEnvAsText "DISCORD_TOKEN"
  let summary =
        T.replace "\n" "\n> " -- quote each line
          . T.replace "&amp;" "&"
          . T.replace "&lt;" "<"
          . T.replace "&gt;" ">"
          $ if T.length (commentBody c) > 200
            then T.take 200 (commentBody c) <> "..."
            else commentBody c
  let message =
        T.concat
          [ "[",
            unCommentID $ commentId c,
            "]",
            " **/u/",
            commentAuthor c,
            ":**\n\n",
            "> ",
            summary,
            "\n\n",
            "<",
            commentUrl c,
            ">" -- the < > stops links from creating embeds
          ]
  let post :: Event -> DiscordHandler ()
      post event = case event of
        Ready {} -> do
          void $
            restCall $
              R.CreateMessageDetailed chanID $
                def
                  { R.messageDetailedContent = message,
                    R.messageDetailedEmbeds = Just []
                  }
          error "done here"
        _ -> pure ()
  void $
    runDiscord $
      def
        { discordToken = discordToken,
          discordOnEvent = post
        }

getRedditEnv :: IO RedditEnv
getRedditEnv = do
  username <- getEnvAsText "REDDIT_ORIG_USERNAME"
  password <- getEnvAsText "REDDIT_ORIG_PASSWORD"
  clientID <- getEnvAsText "REDDIT_ID"
  clientSecret <- getEnvAsText "REDDIT_SECRET"
  let userAgent = "reddit2discord bridge by /u/is_a_togekiss"
  let creds =
        OwnerCredentials
          { ownerUsername = username,
            ownerPassword = password,
            ownerClientId = clientID,
            ownerClientSecret = clientSecret
          }
  authenticate creds userAgent

reddit2Discord :: IO ()
reddit2Discord = do
  env <- getRedditEnv
  runRedditT env $
    stream'
      defaultStreamSettings {streamsStorageSize = 500}
      replyIfPostIdMatches
      id
      (subredditComments 50 subreddit)

getCommentIdFromDiscordMessage :: Discord.Types.Message -> Maybe (ID Comment)
getCommentIdFromDiscordMessage msg =
  let bracketedId = head $ T.words (messageContent msg)
   in case T.unpack bracketedId of
        ['[', c1, c2, c3, c4, c5, c6, c7, ']'] ->
          let commentIdText = T.pack [c1, c2, c3, c4, c5, c6, c7]
           in if T.all (`elem` ("abcdefghijklmnopqrstuvwxyz1234567890" :: String)) commentIdText
                then Just $ CommentID commentIdText
                else Nothing
        _ -> Nothing

discord2Reddit :: IO ()
discord2Reddit = do
  discordToken <- liftIO $ getEnvAsText "DISCORD_TOKEN"
  let handler :: Event -> DiscordHandler ()
      handler e = case e of
        -- Reply to a comment
        MessageCreate m -> do
          case messageReference m >>= referenceMessageId of
            Just repliedToId -> do
              eitherMsg <- restCall $ R.GetChannelMessage (messageChannelId m, repliedToId)
              case eitherMsg of
                Right repliedTo -> do
                  case getCommentIdFromDiscordMessage repliedTo of
                    Just commentId -> do
                      env <- liftIO getRedditEnv
                      runRedditT env $ do
                        upvote commentId
                        addNewComment commentId (messageContent m)
                    Nothing -> pure ()
                Left _ -> pure ()
            _ -> pure ()
        -- Up(down)vote a comment if it gets a thumbs up(down) reaction
        MessageReactionAdd ri -> do
          eitherMsg <- restCall $ R.GetChannelMessage (reactionChannelId ri, reactionMessageId ri)
          case eitherMsg of
            Right reactedTo -> do
              case getCommentIdFromDiscordMessage reactedTo of
                Just commentId -> do
                  env <- liftIO getRedditEnv
                  runRedditT env $ case emojiName (reactionEmoji ri) of
                    -- Not sure which forms of the emoji Discord reports,
                    -- so just catch all
                    "thumbsup" -> upvote commentId
                    "+1" -> upvote commentId
                    "thumbsdown" -> downvote commentId
                    "-1" -> downvote commentId
                    "cross_mark" -> unvote commentId
                    "x" -> unvote commentId
                    _ -> pure ()
                Nothing -> pure ()
            Left _ -> pure ()
        -- Ignore other events
        _ -> pure ()

  void $
    runDiscord $
      def
        { discordToken = discordToken,
          discordOnEvent = handler
        }

main :: IO ()
main = do
  void $ forkIO reddit2Discord
  discord2Reddit
