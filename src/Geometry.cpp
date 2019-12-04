#include <MyCAD/Geometry.hpp>

using namespace MyCAD::Geometry;

Point::Point(float x, float y, float z)
    : myX(x), myY(y), myZ(z)
{}

float Point::x() const
{
    return myX;
}

float Point::y() const
{
    return myY;
}

float Point::z() const
{
    return myZ;
}
