/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include "catch.hpp"

#include <QString>

#include <ostream>

std::ostream& operator<<(std::ostream& ost, QString const& string)
{
    ost << string.toStdString();
    return ost;
}

SCENARIO("We want to test out unit-testing Qt with Catch2", "[GUI]")
{
    GIVEN("a QString")
    {
        QString val("This is a string.");
        WHEN("we copy-construct it")
        {
            QString check(val);
            THEN("we should get the same thing on both sides.")
            {
                REQUIRE(val == check);
            }
        }

        // The test below was writen specifically to fail. I'm commenting it rather than
        // deleting it for no other reason that nostalgia. Feel free to delete it if you
        // wish.
        //WHEN("we have a typo in our test")
        //{
            //QString check("whoops!");
            //THEN("it should fail")
            //{
                //REQUIRE(val == check);
            //}
        //}
    }
}

