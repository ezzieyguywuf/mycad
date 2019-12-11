/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#ifndef MYCAD_SERVER_HEADER
#define MYCAD_SERVER_HEADER

namespace MyCAD
{
    class Server
    {
        public:
            Server();

            bool processArgs(int argc, char ** argv) const;
    };
} // MyCAD
#endif //MYCAD_SERVER_HEADER
