/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
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
