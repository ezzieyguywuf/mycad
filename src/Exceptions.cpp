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

#include <utility> // for std::move

namespace MyCAD{
//=============================================================================
//                        Exception Class Definition
//=============================================================================

/** Any exception thrown from within MyCAD _must_ be MyCAD::Exception or a descendent
 * thereof. The reasoning behind this is in order to make it easy for client code to
 * determine where the exception came from - in other words, something like:
 *
 * ```cpp
 * try
 * {
 *     bool ret = MyCAD::someSortOfFunction();
 * }
 * catch (MyCad::Exception const& e)
 * {
 *      std::cout << "There was an issue in MyCAD!" << std::endl;
 * }
 * ```
 * This will also maybe client errors/exceptions more manageable an meaningful - if the
 * program ever crashes and its MyCAD's fault, it should be immediately clear that this is
 * the case.
 *
 * As often as possible. MyCAD code should check for exceptions (i.e. in CGAL) and wrap it
 * in a MyCAD exception.
 */
Exception::Exception(std::string aMessage)
    : myMessage(std::move(aMessage))
{}

const char* Exception::what() const noexcept
{
    return myMessage.c_str();
}

} // namespace MyCAD
