#include "catch.hpp"

SCENARIO("CAD Programs can create Primitive Solids")
{
    GIVEN("A Box")
    {
        MyCAD::Shape box(10, 10, 10);
        WHEN("the Faces are retrieved")
        {
            const std::vector<MyCAD::Face>& faces = box.getFaces();
            THEN("there should be only 6 faces")
            {
                REQUIRE(faces.size() == 6);
            }
        }
    }
}
