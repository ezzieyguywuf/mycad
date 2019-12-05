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

#include <MyCAD/Exceptions.hpp>

using namespace MyCAD::Exceptions;

//=============================================================================
//                        Exception Class Definition
//=============================================================================

Exception::Exception(std::string const& aMessage)
    : myMessage(aMessage)
{}

Exception::Exception(Exception const& anException)
{
    myMessage = anException.myMessage;
}

Exception& Exception::operator=(Exception const& anException)
{
    myMessage = anException.myMessage;
    return *this;
}

Exception::~Exception(){};

const char* Exception::what() const noexcept
{
    return myMessage.c_str();
}
