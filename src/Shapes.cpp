#include <MyCAD/Shapes.hpp>

using namespace MyCAD::Shapes;

//=============================================================================
//                         Box Class Definition
//=============================================================================
Box::Box(unsigned int /*x*/, unsigned int /*y*/, unsigned int /*z*/)
{
}

std::vector<Face> Box::getFaces() const
{
    return {Face(), Face(), Face(), Face(), Face(), Face()};
}
