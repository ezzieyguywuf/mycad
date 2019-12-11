/* 
 * Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>

 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
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

