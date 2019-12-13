/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#ifndef MYCAD_SHAPES_HEADER
#define MYCAD_SHAPES_HEADER

// All CGAL-related info comes from Geometry
#include "Geometry.hpp"

#include <vector>
#include <ostream>

namespace MyCAD
{
/** @brief Topological shapes - distinctly _not_ geometry */
namespace Shapes
{

/** @brief A point in space */
class Vertex
{
    public:
        explicit Vertex(Geometry::Point const& pnt);
        Geometry::Point const& point() const;

    private:
        Geometry::Point myPoint;
};

/** @brief A bounded piece of a curve */
class Edge
{
    public:
        explicit Edge(Geometry::LineSegment const& aLineSegment);
        Geometry::LineSegment const& getLineSegment() const;

        bool operator==(Edge const& anEdge) const;
        bool operator!=(Edge const& anEdge) const;

    private:
        Geometry::LineSegment myLineSegment;
};

/** @brief A wire is a collection of edges*/
class Wire
{
    public:
        /** @brief Construct a Wire from the given Edge*/
        explicit Wire(std::vector<Edge> const& edges);

        /** @brief Returns the Edges that make up the Wire*/
        std::vector<Edge> getEdges() const;

    private:
        Geometry::Arrangement_2 arr;
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

/** @brief Defines a Geometric Space in which Topological Entities can exist */
class Space
{
    public:
        /** Adds the given Vertex to this Space */
        void addVertex(Vertex vertex);

        /** Returns a vector of all Vertex that currently exist in this Space */
        std::vector<Vertex> const& getVertices() const;

    private:
        std::vector<Vertex> vertices;
};

std::ostream& operator<< (std::ostream& ost, Vertex const& aVertex);
std::ostream& operator<< (std::ostream& ost, Edge const& aWire);
std::ostream& operator<< (std::ostream& ost, Wire const& aWire);

} // namespace Shapes
} // namespace MyCAD

#endif //MYCAD_SHAPES_HEADER
