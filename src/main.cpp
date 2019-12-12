/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include <MyCAD/Communication.hpp>

#include <iostream>

int main(int argc, char* argv[])
{
    MyCAD::Communication::Server myServer;
    myServer.processArgs(argc, argv);

    // Main loop
    while (true)
    {
        std::cout << "$> " ;
        std::string input;
        std::cin >> input;

        MyCAD::Communication::Request request(input);
        bool retval = myServer.processRequest(request);
        if(not retval)
        {
            std::cout << "Error: server didn't like your command \"" << input << "\"" << std::endl;
            continue;
        }
        if(myServer.getResponse() == MyCAD::Communication::Server::EXIT)
        {
            break;
        }
        std::cout << "The server said \"" << myServer.getResponse() << "\"" << std::endl;
    }
    return 0;
}
