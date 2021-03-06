NOTE: This document was originally written when this project was written in
C++. While the general "spirit" of this document still applies, it needs to be
updated to reference haskell things (i.e. haddock instead of doxygen).

In general, we want your code, some come on over!

# Table of Contents
1. [Work Flow](#work-flow)
2. [Code Documentation](#code-documentation)
3. [Header Files](#header-files)
4. [Test-Driven Development (TDD)][#test-driven-development-tdd]
    1. [Writing Tests](#writing-tests)
    2. [Writing Code: first pass](#writing-code-first-pass)
    3. [Writing Code: second pass](#writing-code-second-pass)
    4. [Write More Tests](#writing-more-tests)
5. [TODO File](#todo-file)
6. [Licencing New Files](#licencing-new-files)

## Work Flow [↑][1] <a name="work-flow"></a>

NOTE: I had a nicely updated Work Flow section made up - and then I borked it up with a
silly `git rebase --hard` trying to get fancy with my history re-writes.

Long story short:

1. Always start with an issue (in gitlab)
2. Use the TDD methodolgy outline in this document
3. Use a merge-request to get your code added to master.

One of the User Stories listed in [UserStories][UserStories.md] should be used as the
starting point for most (alL?) new features.

the information gleaned [here][3], [here][4], and [here][5] were referenced in creating
this document, and some others.

to be continued... (need to recover the last info...just rewrite it.)

## Code Documentation [↑][1] <a name="code-documentation"></a>

The header file should contain doxygen-style code documentation. However, the
documentation present in the header should be somewhat minimal. The header documentation
can and should include:

1. A brief description of the class/method/function
2. Documentation for the parameters
3. Documentation for what is returned

Any more detailed description, including documentation of exceptions raised, should go
into the source file.

This should provide a decent balance between readability of the header file (which is more
likely to be references than the source code) while still provide as much detail in the
documentation as is needed.

## Header Files [↑][1] <a name="header-files"></a>

Any header files which describe the public api should be placed in the top-level `include`
directory. Any headers which describe purely "internal" constructs, i.e. stuff that users
don't need to worry about, can be placed somewhere in the `src` tree.

Please note that a "user" here could be an end-user using the software, or a developer
using our code as a library. Keep this in mind when choosing where to put things.

## Test-Driven Development (TDD) [↑][1] <a name="test-driven-development-tdd">

As much as feasible, a Test-Driven Development methodology should be employed. In other
words, the development cycle should go something like this:

1. Write a test that fails (compilation error is a failure)
2. Write the least amount of code needed to make it pass
3. Refactor the code to make it...right

I'll expound on these a bit, but honestly I am no expert.

### Writing Tests [↑][2] <a name="writing-tests"></a>

Tests should be minimal - in other words, consider a feature or behaviour that you need
the program to perform. Now write a test that only tests that feature or behaviour.

Here's an example - let's say we're writing a calculator program. That's a big task. It's
difficult to write a succinct test that "calculates", because there are so many variables
to consider.

Instead, start at step one - we need to be able to provide numbers to the calculator.

So, our first test might be `testGiveDigits` and some pseudocode for the test might look
like:

    Calculator::giveDigits("123");
    assert(Calculator::getDigits(), "123");
    
This will fail for many reasons: first, there is no class defined which is called
Calculator. Next, even if there were, then it wouldn't have the requested methods.

### Writing code: first pass [↑][2] <a name="writing-code-first-pass"></a>

Apparently, we're supposed to forget everythnig we know about good coding practices etc.
here and "just make the test pass!" So let's try that:

```cpp
#include <string>

namespace Calculator
{
    std::string myDigits;
    giveDigits(const std::string& numbs)
    {
        myDigits = numbs;
    }
    std::string getDigits()
    {
        return myDigits;
    }
}

```

If we try this out, I think it will compile. What's more, I think our test wil pass!!!

### Writing Code: second pass [↑][2] <a name="writing-code-second-pass"></a>

Now that we have a passing test (phew!), let's take a step back and observe our creation.
We're using a global variable. Yuck. let's go back and refactor this a bit in order to use
some decent coding practices.

```cpp
#include <string>

class Calculator
{
    public:
        static giveDigits(const std::string& numbs)
        {
            myDigits = numbs;
        }

        const std::string& getDigits() const
        {
            return myDigits;
        }
    private:
        static std::string myDigits;
}
```

This is a bit better. We got rid of some `std::string` copying that was going on, and we
encapsulated the `myDigits` variable in a class. The class is purely static, which doesn't
feel great, but I guess until the tests require us to change it, we'll just leave it
alone...

### Write More Tests [↑][2] <a name="write-more-tests"></a>

As far as I can tell, that's really the gist of TDD. Now just rinse, lather and repeat.

I've actually had limited success using this type of approach, but maybe it's because  I
don't really know what I'm doing. Let's try to use this disciplined, structured approach,
in order to ensure that our code-base is bulletproof!

## TODO File [↑][1] <a name="todo-file"></a>

A  `TODO` will always be present at the top-level of this repository. Given the nature of
how we're approaching TDD, first-passes at writing code can and will likely lead to bad
code. In fact, you may find yourself writing something like:

```cpp
    float MyClass::calculate() const
    {
        return 20;
    }
```

Because you know that this will force the unit-test you wrote earlier to pass. This is 
ok - future unit-tests (motivated by the desire for the calculate function to do more than
just return `20`) will force us to change this.

However, for something as blatantly wrong as this, it should be noted in the `TODO` file
so that we don't lose track of it.

The `TODO` file is _not_ intended to grow forever. Rather, as we enter the "second pass"
of our TDD cycle, we should address some of our issues in the `TODO` file, and erase them
from the `TODO` file as they are resolved.

It is OK if you leave something in `TODO` for a few cycles.

It is _not_ ok to leave somthing in `TODO` forever - either do it, or don't.

## Licencing New Files [↑][1] <a name="licencing-new-files"></a>

The following has been taken directly from the GNU AGPLv3. Any new source files should
contain, at the least, the two lines listed, though preferably the entire block, as the
first lines in the file.

> How to Apply These Terms to Your New Programs
> 
>   If you develop a new program, and you want it to be of the greatest
> possible use to the public, the best way to achieve this is to make it
> free software which everyone can redistribute and change under these terms.
> 
>   To do so, attach the following notices to the program.  It is safest
> to attach them to the start of each source file to most effectively
> state the exclusion of warranty; and each file should have at least
> the "copyright" line and a pointer to where the full notice is found.
> 
>     <one line to give the program's name and a brief idea of what it does.>
>     Copyright (C) <year>  <name of author>
> 
>     This program is free software: you can redistribute it and/or modify
>     it under the terms of the GNU Affero General Public License as published
>     by the Free Software Foundation, either version 3 of the License, or
>     (at your option) any later version.
> 
>     This program is distributed in the hope that it will be useful,
>     but WITHOUT ANY WARRANTY; without even the implied warranty of
>     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
>     GNU Affero General Public License for more details.
> 
>     You should have received a copy of the GNU Affero General Public License
>     along with this program.  If not, see <https://www.gnu.org/licenses/>.
> 
> Also add information on how to contact you by electronic and paper mail.
> 
>   If your software can interact with users remotely through a computer
> network, you should also make sure that it provides a way for users to
> get its source.  For example, if your program is a web application, its
> interface could display a "Source" link that leads users to an archive
> of the code.  There are many ways you could offer source, and different
> solutions will be better for different programs; see section 13 for the
> specific requirements.
> 
>   You should also get your employer (if you work as a programmer) or school,
> if any, to sign a "copyright disclaimer" for the program, if necessary.
> For more information on this, and how to apply and follow the GNU AGPL, see
> <https://www.gnu.org/licenses/>.

[1]: #table-of-contents
[2]: #test-driven-development-tdd
[3]: https://sethrobertson.github.io/GitBestPractices/#read
[4]: http://sethrobertson.github.io/GitPostProduction/gpp.html
[5]: https://nvie.com/posts/a-successful-git-branching-model/
