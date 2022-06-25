module Web.Controller.Posts where

import Web.Controller.Prelude
import Web.View.Posts.Index
import Web.View.Posts.New
import Web.View.Posts.Edit
import Web.View.Posts.Show
import qualified Text.MMark as MMark

import Codec.Crypto.RSA
import System.Random



import Data.Time.Clock
import Data.Time.Calendar

instance Controller PostsController where
    action PostsAction = do
        posts <- query @Post |> orderByDesc #createdAt |> fetch
        let currDate = (getCurrentTime >>= return . toGregorian . utctDay)
        let date2 = (getCurrentTime >>= return . toGregorian . utctDay)
        let isCurrDate = currDate >>= \d1 ->
                         date2 >>= \d2 ->
                            return (d1 == d2)

        let iog =  getStdGen :: IO StdGen 
        let keyPair  = iog >>= \g -> return  (generateKeyPair @StdGen g  1)



        render IndexView { posts = (filter (\x -> True) posts) }

    action NewPostAction = do
        let post = newRecord

        render NewView { .. }

    action ShowPostAction { postId } = --do
        -- post <- fetch postId
        --     >>= fetchRelated #comments
        -- render ShowView { post = post}

        -- fetch postId >>= \post ->
        --     (fetchRelated #comments post) >>= \post2 ->
        --         render ShowView { post = post2}
        do post <- fetch postId
           post2 <- (fetchRelated #comments post)
           render ShowView { post = post2}


    action EditPostAction { postId } = do
        post <- fetch postId
        render EditView { .. }

    action UpdatePostAction { postId } = do
        post <- fetch postId
        post
            |> buildPost
            |> ifValid \case
                Left post -> render EditView { .. }
                Right post -> do
                    post <- post |> updateRecord
                    setSuccessMessage "Post updated"
                    redirectTo EditPostAction { .. }

    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> buildPost
            |> ifValid \case
                Left post -> render NewView { .. } 
                Right post -> do
                    post <- post |> createRecord
                    setSuccessMessage "Post created"
                    redirectTo PostsAction

    action DeletePostAction { postId } = do
        post <- fetch postId
        deleteRecord post
        setSuccessMessage "Post deleted"
        redirectTo PostsAction

buildPost post = post
    |> fill @["title","body"]
    |> validateField #title nonEmpty
    |> validateField #body isMarkdown 

getTitle post = get #title post

isMarkdown :: Text -> ValidatorResult
isMarkdown text =
    case MMark.parse "" text of
        Left _ -> Failure "Please provide valid Markdown"
        Right _ -> Success


-- genCurrKeyPair :: IO (PublicKey, PrivateKey, StdGen)
-- genCurrKeyPair = let iog :: IO StdGen =  getStdGen
--                     in iog >>= \g -> return (generateKeyPair g 1)

