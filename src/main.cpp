#include <MyCAD/Server.hpp>

int main(int argc, char* argv[])
{
    MyCAD::Server myServer;
    myServer.processArgs(argc, argv);
    return 0;
}
