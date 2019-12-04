# Table of Contents
1. [Work Flow](#work-flow)
2. [Code Documentation](#code-documentation)
3. [Header Files](#header-files)
4. [Test-Driven Development (TDD)](#test-driven-development-tdd)
    1. [Writing Tests](#writing-tests)
    2. [Writing Code: first pass](#writing-code-first-pass)
    3. [Writing Code: second pass](#writing-code-second-pass)
    4. [Write More Tests](#writing-more-tests)
5. [Licencing New Files](#licencing-new-files)

## Work Flow [↑](#table-of-contents) <a name="work-flow"></a>

For now, let's keep all active development in the `master_staging` branch - it should be
relatively painless to commit and push these changes to upstream.

However, let's be more selective about when to merge into `master` - the gitlab continuous
integration will only trigger for new commits in `master`. Therefore, while I say "more
selective", that's not to say "don't ever merge to master."

Rather, the workflow should be something like

1. Write new test (See TDD below)
2. Write code, iterate till test passes
3. Update documentation, refactor code
4. Merge into master

This way, the CI is only triggered once per "feature" addition, as defined by the TDD
methodology.

I don't know if this is the best approach or not, but we'll start with this

## Code Documentation [↑](#table-of-contents)

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

## Header Files [↑](#table-of-contents)

Any header files which describe the public api should be placed in the top-level `include`
directory. Any headers which describe purely "internal" constructs, i.e. stuff that users
don't need to worry about, can be placed somewhere in the `src` tree.

Please note that a "user" here could be an end-user using the software, or a developer
using our code as a library. Keep this in mind when choosing where to put things.

## Test-Driven Development (TDD) [↑](#table-of-contents)

As much as feasible, a Test-Driven Development methodology should be employed. In other
words, the development cycle should go something like this:

1. Write a test that fails (compilation error is a failure)
2. Write the least amount of code needed to make it pass
3. Refactor the code to make it...right

I'll expound on these a bit, but honestly I am no expert.

### Writing Tests [↑](#test-driven-development-tdd)

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

### Writing code: first pass [↑](#test-driven-development-tdd)

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

### Writing Code: second pass [↑](#test-driven-development-tdd)

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

### Write More Tests [↑](#test-driven-development-tdd)

As far as I can tell, that's really the gist of TDD. Now just rinse, lather and repeat.

I've actually had limited success using this type of approach, but maybe it's because  I
don't really know what I'm doing. Let's try to use this disciplined, structured approach,
in order to ensure that our code-base is bulletproof!

## Licencing New Files [↑](#table-of-contents)

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
