/* 
 * Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>

 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef MYCAD_SHAPES_HEADER
#define MYCAD_SHAPES_HEADER

#include "Geometry.hpp"

#include <vector>

namespace MyCAD
{
    /** @brief Topological shapes - distinctly _not_ geometry */
    namespace Shapes
    {
        /** @brief A point in space */
        class Vertex
        {
            public:
                Vertex(Geometry::Point const& pnt);
                Geometry::Point const& point() const;

            private:
                Geometry::Point myPoint;
        };

        /** @brief A curve described by a 1D parametric space*/
        class Edge
        {
            public:
                Edge(Geometry::Line const& aLine);
                Geometry::Line const& getLine() const;

                bool operator==(Edge const& anEdge) const;
                bool operator!=(Edge const& anEdge) const;

            private:
                Geometry::Line myLine;
        };

        /** @brief A wire is a collection of edges*/
        class Wire
        {
            public:
                /** @brief Construct a Wire from the given Lines*/
                Wire(std::vector<Geometry::Line> const& lines);

                /** @brief Returns the Edges that make up the Wire*/
                std::vector<Edge> const& getEdges() const;

            private:
                std::vector<Edge> myEdges;
        };

        /** @brief A two-dimensional portion of space */
        class Face
        {
            public:
                Face() = default;
        };

        /** @brief A Solid made of six Faces */
        class Box
        {
            public:
                /** @brief Construct a box with the given dimesions
                 *  @param x,y,z The length of the sides of the box in the given cartesian
                 *               directions
                 */
                Box(unsigned int x, unsigned int y, unsigned int z);
                std::vector<Face> getFaces() const;
        };
    }
}

#endif //MYCAD_SHAPES_HEADER
