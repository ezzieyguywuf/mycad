/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#ifndef MYCAD_COMMANDS_HEADER
#define MYCAD_COMMANDS_HEADER

#include <MyCAD/Shapes.hpp>

namespace MyCAD
{
/** @brief Contains all the Commands that Server can be registered with*/
namespace Commands
{

/** @brief This can be called to register all the commands at runtime */
void RegisterAllCommands();

/** @brief Get the version of the FreeCAD server */
class Version : public Command
{
    public:
        Version();
        ~Version() override = default;

        std::string execute(std::string const& data, Shapes::Space& space) const override;
};

/** @brief Perform various additions of topological entities to Space*/
class Add : public Command
{
    public:
        Add();
        ~Add() override = default;

        std::string execute(std::string const& data, Shapes::Space& space) const override;
};
} // namespace Commands
} // namespace MyCAD

#endif // MYCAD_COMMANDS_HEADER
