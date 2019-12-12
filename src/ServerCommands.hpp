/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#ifndef MYCAD_SERVER_COMMAND
#define MYCAD_SERVER_COMMAND

namespace MyCAD
{
namespace Server
{
namespace Commands
{
/** A Command is an action that Server knows how to perform */
class Command
{
    public:
        /** token CANNOT contain a space */
        Command(std::string token, std::string help="");
        virtual ~Command()=0;

        void getHelp() const;

    private:
        std::string myToken;
        std::string myHelp;
}
} // namespace Commands
} // namespace Server
} // namespace MyCAD

#endif // MYCAD_SERVER_COMMAND
