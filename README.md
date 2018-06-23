# Racket roguelike

In my new project (so new in fact it doesn't even have a proper name yet) I try to
build a [roguelike][1] in [Racket][2]. Racket is a modern and cool lisp dialect and
I chose it because I want to get involved in functional programming, but at the same 
time don't want to struggle with Haskell.

I'm following the [Python 3 libtcod tutorial][3], but without libtcod 
and without Python (yeah). I'll try to stick to the original where possible,
if not with the architecture, then at least with the general idea.

There's also an accompanying blog series about this now, you can [see it here][6]

## The motivation

The biggest motivation for me was the r/roguelikedev's 
[r/roguelikedev does the complete roguelike tutorial][4], which is an annual
event aimed at new (roguelike) developers (like me), provides them motivation
help, and a sense of community. If you're even only remotely interested in
making your own roguelike, be sure to visit that subreddit!

## Play for yourself

If you have Racket installed on your computer, running the game should be pretty straightforward. It was only tested on my Mac running MacOS Mojave, but should run at least on Linux as well.

1. Clone the repo using `git clone https://github.com/Eugleo/roguelike.git`

2. Install [this font][5], or change the used font in `roguelike.rkt`

3. Open terminal, `cd` to the directory of the roguelike and run `racket main.rkt`

4. You can move using vim keys, arrow keys, or numpad. (When I'm writing this, these are actually the only things you can do right now)


[1]: https://en.wikipedia.org/wiki/Roguelike
[2]: https://racket-lang.org
[3]: http://rogueliketutorials.com/libtcod/1
[4]: https://www.reddit.com/r/roguelikedev/comments/8s5x5n/roguelikedev_does_the_complete_roguelike_tutorial/
[5]: https://www.dafont.com/press-start-2p.font
[6]: https://hicsuntleones.netlify.com/tags/roguelike/
