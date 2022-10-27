---
title: How To Automate Spotify Through Alfred
author: sanjiv sahayam
description: how to automate spotify through alfred
tags: alfred, macosx
comments: true
---

I recently found myself thinking it would be cool to control [Spotify](https://www.spotify.com/) through [Alfred](https://www.alfredapp.com/).

![That's Cool](https://media.giphy.com/media/cnuS67F8IoVTYRvJXE/giphy.gif)

I use Alfred as my defacto launcher app and search tool on MacOSX. You can achieve this through Alfred [Workflows](https://www.alfredapp.com/workflows/). One way to do this is by using the [AppleScript](https://developer.apple.com/library/archive/documentation/AppleScript/Conceptual/AppleScriptLangGuide/introduction/ASLR_intro.html) integration provided in the Workflow editor.

![Alfred Workflow Editor](https://www.alfredapp.com/help/workflows/workflow-canvas.png)

See [Learn to Create Workflows with Alfred's Built-In Examples](https://www.alfredapp.com/blog/tips-and-tricks/learn-to-create-workflows-with-alfreds-built-in-examples/) if you're new to creating Alfred Workflows.

The challenging part, for me at least, was crafting the AppleScript necessary. I've documented some scripts below that can be plugged into an Alfred workflow.

The basic outline of an AppleScript to use Spotify is of the form:

```{.command .scrollx}
on run
  tell application "Spotify"
    <your Spotify functionality>
  end tell
end run
```

To see a full list of Spotify functionality supported through AppleScript, do the following:

1. Open the `Script Editor` application
2. Choose `Window` > `Library` from the main menu or use the shortcut `CMD` + `ALT` + `L`
3. If you don't see the Spotify icon in the list, then click on the + icon at the top left of the window to `Add an item to Library`. Browse to the `Application` folder on your machine and choose the Spotify application.

![Spotify Library](/images/how-to-automate-spotify-through-alfred/script-editor-spotify.png)

4. Double click on the Spotify Library to view the available functions:

![Spotify Library](/images/how-to-automate-spotify-through-alfred/script-editor-spotify-suite.png)

## Toggle a Track

Rather than playing and stopping Spotify, my workflow is to toggle playing or stopping a track.

```{.command .scrollx}
on run
  tell application "Spotify"
    playpause
  end tell
end run
```

As expected this would stop a playing track or play a stopped track.

## Next Track

```{.command .scrollx}
on run
  tell application "Spotify"
    next track
  end tell
end run
```

## Previous Track

```{.command .scrollx}
on run
  tell application "Spotify"
    previous track
  end tell
end run
```

## Playing an Album

The general syntax is for the AppleScript automation is:

```{.command .scrollx}
on run
  tell application "Spotify"
        play track "spotify:track:<first_track_id>" in context "spotify:album:<album_id>"
  end tell
end run
```

Copy the album link as right-clicking on the ellipses next to the album and choosing `Share` > `Copy Album Link`.

![Album](/images/how-to-automate-spotify-through-alfred/album_id.png)

The link will be of the form:

```{.terminal .scrollx}
https://open.spotify.com/album/41Ywc4XSD3GChkeAfzhRIR?si=htPbJS67QB6GapPw-p6W0w
```

The `album_id` in the above link is `41Ywc4XSD3GChkeAfzhRIR` and as usual we can ignore everything else.


The `first_track_id` of the album can be found by clicking on the ellipses near the right of the first track of the album, and choosing `share` > `Copy Song Link`

![First Track of Album](/images/how-to-automate-spotify-through-alfred/album_track_id.png)

The link will be of the form:

```{.terminal .scrollx}
https://open.spotify.com/track/7tWyfy3ySgLMbH9R0xeqzZ?si=20d78c906e7e462f
```

The first_song_id is `7tWyfy3ySgLMbH9R0xeqzZ` in the above link.

Putting it all together we get:

```{.command .scrollx}
on run
  tell application "Spotify"
        play track "spotify:track:7tWyfy3ySgLMbH9R0xeqzZ" in context "spotify:album:41Ywc4XSD3GChkeAfzhRIR"
  end tell
end run
```


## Playing a Playlist

The general syntax is for the AppleScript automation is:

```{.command .scrollx}
on run
  tell application "Spotify"
        play track "spotify:track:<first_track_id>" in context "spotify:user:<user_id>:playlist:<playlist_id>"
  end tell
end run
```

The `user_id` is the owner of the playlist. More on that later.

It's also important to note that you need to specify the playlist **and** the first track you want to start playing when you run the playlist. The track has to be played `in context` of the playlist.

![Shrug](https://media.giphy.com/media/720g7C1jz13wI/giphy.gif)

### Spotify Playlist

For the example below I will be using the [The Piano Bar](https://open.spotify.com/playlist/37i9dQZF1DWVvXA824aCbn?si=e31800da15c0430e) playlist is created by Spotify.

![The Piano Bar Playlist](/images/how-to-automate-spotify-through-alfred/public-playlist.png)

The `user_id` of any user can be retrieved by right-clicking on the user name of the owner of the playlist and selecting `Copy link to profile`. The `user_id` can be extracted from the link copied as described below.

![Spotify User](/images/how-to-automate-spotify-through-alfred/spotify-user.png)

The link is of the form:

```{.terminal .scrollx}
https://open.spotify.com/user/spotify?si=ba05be61cab84242
```

The `user_id` from the above link is `spotify`. The `?si=` part can be ignored for our purposes.

The displayed user name and the actual `user_id` can be different. This is why we try to find the actual `user_id` from the profile link.


The `playlist_id` can be found by clicking on the ellipses near the top left of the playlist, and choosing `share` > `copy link to playlist`

![Playlist Share Link](/images/how-to-automate-spotify-through-alfred/playlist_id.png)

The share link is:

```{.terminal .scrollx}
https://open.spotify.com/playlist/37i9dQZF1DWVvXA824aCbn?si=04e7f5def59f4f93
```

The playlist id in the above url is `37i9dQZF1DWVvXA824aCbn`. The `?si=` part can be ignored for our purposes.

The `first_track_id` of the playlist can be found by clicking on the ellipses near the right of the first track of the playlist, and choosing `share` > `Copy Song Link`

![Track Share Link](/images/how-to-automate-spotify-through-alfred/first_track_id.png)


The share link to the first track is:

```{.terminal .scrollx}
https://open.spotify.com/track/1nsMASRDWwUwlBMmOuh80d?si=eaa86c63aa504858
```

The `first_track_id` in the above url is: `1nsMASRDWwUwlBMmOuh80d`. The `?si=` part can be ignored for our purposes.

Below is the ApplesSript code, to run the `The Piano Bar` playlist using the attributes: `first_track_id`, `playlist_id` and `user_id`, collected above:

```{.command .scrollx}
on run
  tell application "Spotify"
        play track "spotify:track:1nsMASRDWwUwlBMmOuh80d" in context "spotify:user:spotify:playlist:37i9dQZF1DWVvXA824aCbn"
  end tell
end run
```

As a second example we can take my [Folk](https://open.spotify.com/playlist/7znakCCH9jS9fpl0GmJYah?si=78fb6db9954a41ba) playlist:

![Folk Playlist](/images/how-to-automate-spotify-through-alfred/folk-playlist.png)

Right click on the user name and select `Copy link to profile`.

The link will be of the form:

```{.terminal .scrollx}
https://open.spotify.com/user/ssanjs?si=0c84ec2c4811471c
```

In the above link, my Spotify user id is `ssanjs` and as usual we can ignore everything else.

Given the following attributes:

|Attibute|Value|
|--|--|
|user|ssanjs|
|playlist_id|7znakCCH9jS9fpl0GmJYah|
|first_song_id|5PAnntRTBk6qgst5Fw84Y8|

Our script is:

```{.command .scrollx}
on run
  tell application "Spotify"
       play track "spotify:track:5PAnntRTBk6qgst5Fw84Y8" in context "spotify:user:ssanj:playlist:7znakCCH9jS9fpl0GmJYah"
  end tell
end run
```

## Magic Happens

Once you've setup your Alfred Workflows for Spotify you can simply change to your favourite album or playlist from the search box:

![Folk Playlist](/images/how-to-automate-spotify-through-alfred/alfred-spotify.gif)


## Links

- [Applescript play track spotify](https://stackoverflow.com/questions/11607618/applescript-play-track-spotify)
 - [Sample AppleScript to Control Spotify](https://gist.github.com/NoobsArePeople2/5121597)
 - [Controlling Spotify with AppleScript](https://stackoverflow.com/questions/8901556/controlling-spotify-with-applescript)

