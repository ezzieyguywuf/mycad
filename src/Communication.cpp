/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#include <MyCAD/Geometry.hpp>
#include <MyCAD/Communication.hpp>

#include "cxxopts.hpp"

#include <utility> // for std::move

namespace MyCAD
{
/** This namespace describes the fundamental userspace of MyCAD. In a nutshell, MyCAD is
 *  designed from the beginning to work in a server-client model. The classes in this
 *  namespace describe that server-client relationship, as well as how to communicate
 *  between the two.
 */
namespace Communication
{

namespace
{
    cxxopts::Options OPTIONS("MyCAD", "A Computer Aided Design program.");
} // namespace

//=============================================================================
//                      Request Class Definition
//=============================================================================
/** A Request is defined as a "command" (currently a simple std::string). A "command" is
 *  something that MyCAD::Server knows how to interpret. Upon receiving this "command",
 *  MyCAD::Server will execute whatever code it needs to and then store a "response" for
 *  the requestor to later query.
 *
 *  In the future, it may be desireable to abstract away this concept of a "command",
 *  perhaps into a Command class.
 */
Request::Request(std::string aRequest)
    : myRequest(std::move(aRequest))
{}

std::string const& Request::get() const
{
    return myRequest;
}

//=============================================================================
//                       Server Class Definition
//=============================================================================
/** A Server knows how to process various Request and do something with them. In general,
 *  this will probably mean creating/manipulating/querying topological or geometric
 *  information.
 *
 *  Server understands various command-line arguments which can be used to initialize/set
 *  various internal variables. While Server does not provide a main-loop, it is rather
 *  trivial to create one which leverages Server. Make sure to call Server::processArgs if
 *  you're interested in accepting command-line input from Users.
 */
Server::Server()
{
    static bool first = true;
    if (first)
    {
        first = false;
        OPTIONS.add_options()
            ("v,version", "Return the version of the MyCAD server")
            ;
    }
}

/** This will process the list of provided command-line arguments. If there is an error,
 *  the caller is notified by a return value of false.
 *
 *  @warning The caller should check the return value and respond appropriately, otherwise
 *           Server will contain an unknown (to the caller) state
 */
bool Server::processArgs(int argc, char ** argv) const
{
    try{
        cxxopts::ParseResult result = OPTIONS.parse(argc, argv);
        if(result.count("version") > 0)
        {
            std::cout << "MyCAD©, v" MYCAD_VERSION << std::endl;
        }
    }
    catch (cxxopts::OptionParseException const& e)
    {
        std::cout << e.what() << std::endl;
        return false;
    }
    return true;
}

/** Given the Request, perform the requested action.
 *
 *  A return value of `false` indicates that there was an error processing the request
 */
bool Server::processRequest(Request const& request)
{
    std::string data = request.get();
    std::stringstream ss(data);
    std::string command;
    ss >> command;

    if(command == "version")
    {
        myResponse = std::string(MYCAD_VERSION);
    }
    else if(command == "vertex")
    {
        // Extract the user's targets
        MyCAD::Geometry::Number x,y;
        ss >> x >> y;

        // Create the vertex
        vertices.emplace_back(MyCAD::Geometry::Point(x, y));

        // Let the user know everything went well.
        std::stringstream oss;
        oss << "Added vertex at " << vertices.back();
        myResponse = oss.str();
    }
    else if(data == "exit" or data == "quit")
    {
        myResponse = EXIT;
    }
    else{
        return false;
    }
    return true;
}

/** Returns the response from the last succesfully processed Request.
 *
 *  @warning Server does not know anything about the last Request at this point, or even
 *           if we've received a request yet. In other words, the caller must ensure that:
 *
 *           1. They have actually sent a Request prior to calling `getResponse`
 *           2. That the last request was processed succesfully prior to calling
 *           `getRespons`
 *
 *           If these two things are not done, bad things won't happen. But, you'll
 *           either:
 *
 *           1. Get an empty string as a response. The caller __must only__ rely on this
 *              empty string as a true response iff they did the two things mentioned
 *              above.
 *           2. Get the response from the previously succesful processing, which could
 *              lead to surprising results on your end.
 */
std::string Server::getResponse() const
{
    return myResponse;
}
} // Communication
} // MyCAD
