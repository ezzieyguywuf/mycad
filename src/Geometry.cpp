#include <MyCAD/Geometry.hpp>

using namespace MyCAD::Geometry;

//=============================================================================
//                        Point Class Definition
//=============================================================================
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

bool Point::operator==(Point const& aPoint) const
{
    return myX == aPoint.myX and myY == aPoint.myY and myZ == aPoint.myZ;
}

bool Point::operator!=(Point const& aPoint) const
{
    return not (*this == aPoint);
}

//=============================================================================
//                        Line Class Definition
//=============================================================================

Line::Line(Point const& /*p1*/, Point const& /*p2*/)
{
}

float Line::getLowerParameter() const
{
    return 0;
}

float Line::getUpperParameter() const
{
    return 10;
}

bool Line::operator==(Line const& aLine) const
{
    return (this->getLowerParameter() == aLine.getLowerParameter() and 
            this->getUpperParameter() == aLine.getUpperParameter());
}

bool Line::operator!=(Line const& aLine) const
{
    return not (*this == aLine);
}
