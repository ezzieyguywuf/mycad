/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#ifndef MYCAD_EXCEPTIONS_HEADER
#define MYCAD_EXCEPTIONS_HEADER

#include <exception>
#include <string>

namespace MyCAD
{
/** @brief A concrete implementation of std::exception */
class Exception : public std::exception
{
    public:
        explicit Exception(std::string message);

        /** @brief Return the mesage we were instantiated with */
        const char* what() const noexcept override;

    private:
        std::string myMessage;
};
} // namespace MyCAD
#endif // MYCAD_EXCEPTIONS_HEADER
