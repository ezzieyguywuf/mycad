/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include "cxxopts.hpp"

#include <MyCAD/Communication.hpp>

#include <iostream>
#include <string>  // for std::getline

bool parseCommandLineArguments(MyCAD::Communication::Server& server, int argc, char ** argv)
{
    cxxopts::Options options("MyCAD", "A Computer Aided Design program.");

    // Tese are the command-line options we understand
    options.add_options()
        ("v,version", "Return the version of the MyCAD server")
        ;

    try{
        cxxopts::ParseResult result = options.parse(argc, argv);
        if(result.count("version") > 0)
        {
            std::cout << server.processRequest("version") << std::endl;
        }
    }
    catch (cxxopts::OptionParseException const& e)
    {
        std::cout << e.what() << std::endl;
        return false;
    }
    return true;
}

int main(int argc, char* argv[])
{
    // Instantiate our server and send it to the command-line parser
    MyCAD::Communication::Server myServer;
    bool ret = parseCommandLineArguments(myServer, argc, argv);

    if(not ret)
    {
        std::cout << "exiting." << std::endl;
        return 1;
    }

    // Main loop
    while (true)
    {
        std::cout << "$> " ;
        std::string input;
        std::getline(std::cin, input);

        std::cout << myServer.processRequest(input) << std::endl;
        if(myServer.shutdown())
        {
            break;
        }
    }
    return 0;
}
