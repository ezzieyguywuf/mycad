/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include <MyCAD/Communication.hpp>

int main(int argc, char* argv[])
{
    MyCAD::Communication::Server myServer;
    myServer.processArgs(argc, argv);
    return 0;
}
