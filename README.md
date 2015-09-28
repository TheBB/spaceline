# Spaceline

[![MELPA](http://melpa.org/packages/spaceline-badge.svg)](http://melpa.org/#/spaceline)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Spaceline](#spaceline)
    - [Introduction](#introduction)
    - [The Spacemacs mode-line theme](#the-spacemacs-mode-line-theme)
        - [Why does it look different?](#why-does-it-look-different)
    - [Medium tweaking](#medium-tweaking)
        - [Turning segments on and off](#turning-segments-on-and-off)
        - [The highlight face](#the-highlight-face)
        - [Other faces](#other-faces)
        - [Powerline separators](#powerline-separators)
        - [Hooks](#hooks)
        - [Unicode numbers](#unicode-numbers)
        - [Minor modes separator](#minor-modes-separator)
        - [Org clock](#org-clock)
    - [Difficult tweaking](#difficult-tweaking)
        - [Segments](#segments)
        - [Defining a segment](#defining-a-segment)
        - [Properties](#properties)
        - [Bindings](#bindings)
        - [Setting up a mode-line](#setting-up-a-mode-line)

<!-- markdown-toc end -->

![Screenshot](screen.png)

This is the package that provides
[Spacemacs](https://github.com/syl20bnr/spacemacs) with its famous mode-line
theme. It has been extracted as an independent package for general fun and
profit.

## Introduction

This package provides features for three kinds of users.

1. You just want to use the Spacemacs mode-line theme and forget about it.
2. You want to use something similar to the Spacemacs mode-line theme, but with
a handful of easy tweaks.
3. You want an easy-to-use library for building your own mode-line from scratch,
and you think the Spacemacs theme looks good.

The functionality for each are described in the following sections.

The files in this package are organized as follows. Choose which you want to
load based on what you want to do.

- `spaceline.el`: Contains the core library used to define segments and render
the modeline. It defines no segments by itself except the `global` segment. (See
below.)
- `spaceline-segments.el`: Defines all the segments used by the default
Spacemacs theme, but doesn't do anything with them.
- `spaceline-config.el`: Defines the default Spacemacs theme.

## The Spacemacs mode-line theme

To install it, just load `spaceline-config` and call the theme function you
want. E.g.

    (require 'spaceline-config)
    (spaceline-spacemacs-theme)

There are two themes currently:

- `spaceline-spacemacs-theme`: The theme used by Spacemacs.
- `spaceline-emacs-theme`: A similar theme designed to look good with no other
packages installed.

It should work out of the box. This theme integrates with the following
third-party packages, which you may consider installing for added benefit:

- [`eyebrowse`](https://github.com/wasamasa/eyebrowse): a simple workspace-like
package.
- [`window-numbering`](https://github.com/nschum/window-numbering.el): gives
each visible window a number.
- [`anzu`](https://github.com/syohex/emacs-anzu): shows the current match and
the total number of matches while searching.
- [`flycheck`](https://github.com/flycheck/flycheck/): shows the number of
errors, warnings and notifications after syntax checking.
- [`erc`](http://www.emacswiki.org/emacs/ERC): shows the channels with new
messages if you have `erc-track` turned on.
- [`org`](http://orgmode.org/): shows the currently clocking task.
- [`org-pomodoro`](https://github.com/lolownia/org-pomodoro): kind of like
clocks in org, only with a tomato.
- [`nyan-mode`](https://github.com/TeMPOraL/nyan-mode): shows the current
position in the buffer with kittens and rainbows.
- [`fancy-battery`](https://github.com/lunaryorn/fancy-battery.el): shows
battery information.
- [`evil`](https://bitbucket.org/lyro/evil/wiki/Home): makes Emacs behave like
vim.

### Why does it look different?

There are a number of reasons why Spaceline might look different on your setup
compared to Spacemacs proper. Some of the more important ones are addressed
here.

- You're missing an optional dependency. Spacemacs includes packages that
display information in the mode-line. The leftmost segment is invisible if
`eyebrowse-mode`, `window-numbering-mode` and `evil` are all not present. If you
don't wish to use these packages, there is a theme provided called
`spaceline-emacs-theme` which is supposed to look good regardless.

- Consider setting or increasing the value of `powerline-height` to give your
mode-line some room to breathe.

- The default powerline separator is `arrow`, but Spacemacs uses `wave`. You
should try out various settings of `powerline-default-separator` to find the one
that works for you.

- If you're using `eyebrowse-mode` or `window-numbering-mode`, consider setting
`spaceline-workspace-numbers-unicode` and `spaceline-window-numbers-unicode` to
`t` to get the nice-looking unicode numbers seen in the screenshot.

- Use [`diminish`](https://github.com/emacsmirror/diminish) to tweak the output
of the minor modes segment.

- To get the mode-line highlight to change color depending on the evil state,
set `spaceline-highlight-face-func` to `spaceline-highlight-face-evil-state`. 

## Medium tweaking

### Turning segments on and off

Each segment has a variable `spaceline-<name>-p` that can switch the segment off
by setting it to `nil`. There are also three convenient interactive functions
for toggling segments:

- `spaceline-toggle-<name>`
- `spaceline-toggle-<name>-on`
- `spaceline-toggle-<name>-off`

You can bind these to whichever keys you like.

The full list of segments available, from left to right:

- `workspace-number`: integrates with `eyebrowse`.
- `window-number`: integrates with `window-numbering`.
- `evil-state`: shows the current evil state, integrates with `evil`.
- `anzu`: integrates with `anzu`.
- `buffer-modified`: the standard marker denoting whether the buffer is modified
or not.
- `buffer-size`: the size of the buffer.
- `buffer-id`: the name of the buffer.
- `remote-host`: the host for remote buffers.
- `major-mode`: the current major mode.
- `flycheck-error`: number of flycheck errors, integrates with `flycheck`.
- `flycheck-warning`: number of flycheck warnings, integrates with `flycheck`.
- `flycheck-info`: number of flycheck notifications, integrates with `flycheck`.
- `minor-modes`: the currently enabled minor modes. The output of this segment
can be tweaked with [`diminish`](https://github.com/emacsmirror/diminish).
- `process`: the background process associated with the buffer, if any.
- `erc-track`: IRC channels with new messages, integrates with `erc`.
- `version-control`: version control information.
- `org-pomodoro`: integrates with `org-pomodoro`.
- `org-clock`: the current org clock, integrates with `org`.
- `nyan-cat`: integrates with `nyan-mode`.
- `battery`: integrates with `fancy-battery-mode`.
- `selection-info`: information about the currently active selection, if any.
- `buffer-encoding-abbrev`: the line ending convention used in the current
buffer (`unix`, `dos` or `mac`).
- `point-position`: the value of point, this is disabled by default.
- `line-column`: current line and column.
- `global`: meta-segment used by third-party packages.
- `buffer-position`: shows the current position in the buffer as a percentage.
- `hud`: shows the currently visible part of the buffer.

There is also a `buffer-encoding` segment, which is not used by the Spacemacs
theme.

### The highlight face

The highlight face is the face that (by default) is a sharp orange, used e.g. by
the HUD segment on the far right, and the first segment on the left (note that
it may be invisible if you are using the Spacemacs theme but not some of its
optional dependencies). The actual face used as a highlight face is determined
by a function, which can be configured by setting the value of
`spaceline-highlight-face-func`. Spaceline comes with three choices, but of
course you can write your own:

- `spaceline-highlight-face-default`: Uses the orange, all the time. This is the
default. Hence the name.
- `spaceline-highlight-face-evil-state`: Chooses a face determined by the
current evil state. The face corresponding to each state is determined by the
association list `spaceline-evil-state-faces`, which contains default values for
the standard evil states. (Spacemacs has a few more.)
- `spaceline-highlight-face-modified`: Chooses a face determined by the status
of the current buffer (modified, unmodified or read-only).

Note that the highlight face is only used in the active window.

### Other faces

In the active window, the mode-line will use these faces:

- `powerline-active1`
- `powerline-active2`
- `mode-line`

And in inactive windows:

- `powerline-inactive1`
- `powerline-inactive2`
- `mode-line-inactive`

### Powerline separators

Set `powerline-default-separator` to configure this. The docstring for that
variable enumerates the choices.

### Hooks

The hook `spaceline-pre-hook` is executed before rendering the modeline. Don't
put any performance-intensive functions here!

### Unicode numbers

By default, Spacemacs displays window numbers and workspace numbers in nice
unicode symbols. To do this in Spaceline, set `spaceline-window-numbers-unicode`
or `spaceline-workspace-numbers-unicode` to true, respectively.

Spacemacs also does this with most minor modes. This is a feature that has not
been ported to Spaceline. To do this, use
[`diminish`](https://github.com/emacsmirror/diminish).

### Minor modes separator

To configure the separator between the minor modes, use
`spaceline-minor-modes-separator`.

### Org clock

The displayed value of the `org-clock` segment is determined by the function
`org-clock-get-clock-string` by default. To configure another function, use
`spaceline-org-clock-format-function`.

## Difficult tweaking

To understand how to do this, we must first understand how Spaceline constructs
a mode-line.

### Segments

A *segment* is any part of the mode-line that produces some kind of visible
output. Typically, segments have been defined ahead of time using
`spaceline-define-segment`, in which case the segment is referred to by a
symbol, but segments may also be literals (strings or numbers, say) or lists of
other segments.

These are all valid segments, provided `my-segment` has been defined:

    my-segment
    "alfa"
    (my-segment 89)

Segments may also have properties associated with them. Spaceline supports a
variety of properties. They can be applied as follows, for a 'singleton'
segment:

    (my-segment :prop-a value-a :prop-b value-b ...)

Or for a list of segments:

    ((my-segment 89)
     :prop-a value-a
     :prop-b value-b)

### Defining a segment

Use `spaceline-define-segment` to define a segment and associate it to a symbol.

    (spaceline-define-segment name
      "Docstring"
      ;; A single form whose value is the value of the segment.
      ;; It may return a string, an image or a list of such.

      ;; Additional keyword properties go here
      )

In addition to storing the segment, this macro produces a variable called
`spaceline-<name>-p` whose value may be set to switch the segment off or on
manually. Three interactive functions are also defined:

- `spaceline-toggle-<name>`
- `spaceline-toggle-<name>-on`
- `spaceline-toggle-<name>-off`

These are convenient to bind to keys, and they do what it says on the tin.

### Properties

The valid properties are

- `:when`: A form that, if it evalutes to `nil`, will block this segment from
being displayed.
- `:separator`: A separator inserted between each element of the value of the
given segment. This makes most sense for lists of segments, or segments whose
values are typically lists (such as `minor-modes`).
- `:fallback`: A segment which will be displayed in place of the current segment
if it should produce no output (either due to a nil `:when` condition or because
the return value of the segment itself is `nil`)
- `:face`: The face in which to render the segment. It may be better to use this
than (or in addition) to propertizing the output directly, since Spaceline needs
to know the faces to propertize the separators correctly. This may be either a
face or a form evaluating to a face.
- `:tight`: Set to `t` to tell Spaceline that the segment should not have any
padding on the right or left. Use `:tight-left` and `:tight-right` for even
finer control.

All of the above are valid both in `spaceline-define-segment` as well as the
more general segment specification given above.

Additionally, `spaceline-define-segment` allows two additional properties.

- `:enabled`: Sets the initial value of the toggle variable.
- `:global-override`: Many third-party packages provide mode-line information by
inserting a segment in the list `global-mode-string`. Sometimes you might like
to write your own segment for this, in which case you have to prevent the
package from using `global-mode-string`, or you will end up with duplicate
information and a crowded mode-line. To do this, set `:global-override` to the
symbol (or list of symbols) which you want to exclude from `global-mode-string`.
This setting will be honored by the `global` segment, which is defined by
Spaceline.

The properties which take effect for any given segment are, in order of
priority:

- the properties specified in the segment specification
- the propreties given in the call to `spaceline-define-segment`
- the properties of the parent segment

The exceptions are `:when`, which must be true on *all* levels for a segment to
be displayed, and `:fallback` which does *not* pass through from the parent
segment.

### Bindings

When evaluating a segment, its `:when` condition or its `:face` property, the
following bindings are available for convenience.

- `active`: Whether the current window is active or not. Many segments use
`:when active` to only show in the current window.
- `default-face`: The face with which the current segment *should* be rendered.
If you don't define a `:face`, this is what you get. For best results, stick to
the default face as often as you can.
- `other-face`: The alternating default face. Spaceline switches `default-face`
and `other-face` for each top-level segment.
- `highlight-face`: The face used to highlight 'important' parts, whatever that
may be. This may be customized.
- `line-face`: The face with which the empty part in the middle of the mode-line
will be rendered.

### Setting up a mode-line

When you have finished defining your segments, call `spaceline-install`. It
expects two arguments, `left` and `right`, which are lists of segments.

For example, this is the Spacemacs mode-line.

    (spaceline-install

     '(((workspace-number window-number)
       :fallback evil-state
       :separator "|"
       :face highlight-face)
      anzu
      (buffer-modified buffer-size buffer-id remote-host)
      major-mode
      ((flycheck-error flycheck-warning flycheck-info)
       :when active)
      (((minor-modes :separator spaceline-minor-modes-separator)
        process)
       :when active)
      (erc-track :when active)
      (version-control :when active)
      (org-pomodoro :when active)
      (org-clock :when active)
      nyan-cat)

     `((battery :when active)
       selection-info
       ((buffer-encoding-abbrev
         point-position
         line-column)
        :separator " | ")
       (global :when active)
       buffer-position
       hud)))

Spaceline will then collect data from each segment and render them in a
beautiful manner, with powerline separators inserted between each top-level
segment. Top-level segments will be rendered with alternating faces.

Note: For facilitating third-party packages, it is strongly recommended that
your mode-line specification includes the `global` segment, which is defined in
the core `spaceline.el` file.
