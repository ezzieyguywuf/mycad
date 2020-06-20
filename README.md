MyCAD
=====

MyCAD is a small passion-project (for now). I like to contribute to
[FreeCAD](https://freecadweb.org), which is a great piece of software with a fantastic
community. However, I often find myself frustrated with the almost 20-year-old code base,
specifically when I find examples of anywhere from poor to downright dangerous coding
practices.

I issue no blame at all on the FreeCAD development team - the project started in the early
2000's, a time when c++ (and many things!) was a much different thing. Also, it is run by
a group of volunteers - we all do our best.

However, in an attempt to satisfy the OCD in me, I'm going to try a "ground up" approach
to what I think a CAD package should/could be.

Project Status
==============

mycad continues to be a work in progress. Most recently, we've transitioned almost all of
the original c++ stuff over to Haskell. Once this transition is entirely complete we'll
continue moving forward with executing the Storied as described below.

One of the goals (number 3 below) is to be disciplined in our approach to growing the code
base. As part of that, I'm trying out some of these fancy "Agile" principles, namely, I'm
maintaining a list of "Stories" (see [Stories.md](Stories.md)) which each represent a
specific feature that a specific User will be interested in.

Importantly, it is generally not OK for a Story to have "A developer" as the User (you'll
notice I've been very careful not to do that).

Hopefully, this approach will mean that development will focus mostly on outward facing
features that you care about - i.e., our demo gif should get neater and neater!

Periodically, though, we'll go back and refactor or rethink parts of the code base, in
order to make it more manageable to bring you new features.

Goals
=====

In general, I have some very concrete goals for this project:

1. Learn - I am not a trained programmer. I've learned what I know on the internet.
   Therefore, one of the first goals is to simply advance my knowledge of coding practices
   and paradigms.
2. Have Fun - I mean, come on: if you're not having fun, what are you doing?!
3. Be Disciplined - I want to be sure that whatever code is pushed to the master branch is
   solid, robust, and maintainable. I think the best way to accomplish this is with
   discipline, whether via coding practices, testing techniques, etc.
4. Document - my favourite libraries to use are those that are documented well. Therefore
   I want to provide that same opportunity to potential users of MyCAD.

How To Build
============
**TODO**: Make this section more better.

MyCAD is being developped using haskell's
[stack](https://docs.haskellstack.org/en/stable/README/), which makes building
pretty easy.

Aside from stack (which will handle all the haskell dependencies), you also need to have a copy of the [glfw library](https://www.glfw.org/) installed in your PATH somewhere - there appear to be windows and mac precompiled libraries, and if you're on linux (like me) just use your regular means to install.

Once all that is out of the way, you can build as follows:

```sh
git clone https://gitlab.com/ezzieyguywuf/mycad
cd mycad
stack build
```

To run the executables, you would:

```sh
stack exec HaskellCAD-exe  # for the REPL command line
stack exec GUI             # for the GUI
```

The GUI is a work in progress, and is currently only a demo of some silly
shapes. It's 3D though, drag your mouse and scroll to see!

that's all it takes! Now, mind you, this project is still very much a work in
progress, but check out the unit test definitions in the `test` sub-directory
to get an idea of how things work and go together.

Credits
=======

The project was developed using the following open-source projects (THANK YOU!)
- [haskell](https://www.haskell.org)
- [glfw](https://www.glfw.org/)
- [gentoo](https://www.gentoo.org/)
- [neovim](https://neovim.io/)
- [firefox](https://www.mozilla.org/en-US/firefox/new/)
- [st](https://st.suckless.org/) (the humble terminal)
- [tmux](https://github.com/tmux/tmux) (makes the terminal sligthly cocky)

This project has also drawn influence and inspiration from:

- [FreeCAD](https://www.freecadweb.org/)
- [Blender](https://www.blender.org/)
