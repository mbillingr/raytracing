use crate::shapes::{smooth_triangle, triangle, Group, SceneItem};
use crate::tuple::{point, vector, Point, Vector};
use std::collections::BTreeMap;

pub struct ObjParser<'a> {
    ignored: Vec<&'a str>,
    vertices: Vec<Point>,
    normals: Vec<Vector>,
    groups: BTreeMap<&'a str, Group>,
    current_group: &'a str,
}

impl<'a> ObjParser<'a> {
    fn empty() -> Self {
        ObjParser {
            ignored: vec![],
            vertices: vec![],
            normals: vec![],
            groups: {
                let mut gs = BTreeMap::new();
                gs.insert("default_group", Group::default());
                gs
            },
            current_group: "default_group",
        }
    }

    pub fn parse_str(input: &str) -> ObjParser {
        let mut data = ObjParser::empty();

        for line in input.lines() {
            let line = line.trim();

            match line {
                _ if line.starts_with("vn") => data.parse_normal(line),
                _ if line.starts_with('v') => data.parse_vertex(line),
                _ if line.starts_with('f') => data.parse_face(line),
                _ if line.starts_with('g') => data.parse_group(line),
                _ => data.ignored.push(line),
            }
        }

        data
    }

    pub fn ignored(&self) -> &[&str] {
        &self.ignored
    }

    pub fn get_group(&self, group: &str) -> &Group {
        &self.groups[group]
    }

    fn parse_vertex(&mut self, line: &str) {
        let v: Vec<f64> = line
            .split_whitespace()
            .skip(1)
            .map(str::parse)
            .filter_map(Result::ok)
            .collect();
        self.vertices.push(point(v[0], v[1], v[2]))
    }

    fn parse_normal(&mut self, line: &str) {
        let v: Vec<f64> = line
            .split_whitespace()
            .skip(1)
            .map(str::parse)
            .filter_map(Result::ok)
            .collect();
        self.normals.push(vector(v[0], v[1], v[2]))
    }

    fn parse_face(&mut self, line: &str) {
        let elements: Vec<_> = line.split_whitespace().skip(1).collect();
        if elements[0].contains('/') {
            let idx: Vec<_> = elements
                .into_iter()
                .map(|s| {
                    let mut s = s.split('/');
                    let i = s.next().unwrap();
                    let _ = s.next().unwrap();
                    let n = s.next().unwrap();
                    (i.parse().unwrap(), n.parse().unwrap())
                })
                .map(|(i, n): (usize, usize)| (i - 1, n - 1))
                .collect();
            self.triangulate_smooth_fan(&idx);
        } else {
            let idx: Vec<_> = elements
                .into_iter()
                .map(str::parse)
                .filter_map(Result::ok)
                .map(|i: usize| i - 1)
                .collect();
            self.triangulate_fan(&idx);
        }
    }

    fn parse_group(&mut self, line: &'a str) {
        let name = line
            .split_whitespace()
            .skip(1)
            .next()
            .unwrap_or("default_group");
        self.current_group = name;
        if !self.groups.contains_key(name) {
            self.groups.insert(name, Group::default());
        }
    }

    fn triangulate_fan(&mut self, idx: &[usize]) {
        for i in 1..idx.len() - 1 {
            let tri = triangle(
                self.vertices[idx[0]],
                self.vertices[idx[i]],
                self.vertices[idx[i + 1]],
            );
            self.groups
                .get_mut(&self.current_group)
                .unwrap()
                .add_child(tri);
        }
    }

    fn triangulate_smooth_fan(&mut self, idx: &[(usize, usize)]) {
        for i in 1..idx.len() - 1 {
            let tri = smooth_triangle(
                self.vertices[idx[0].0],
                self.vertices[idx[i].0],
                self.vertices[idx[i + 1].0],
                self.normals[idx[0].1],
                self.normals[idx[i].1],
                self.normals[idx[i + 1].1],
            );
            self.groups
                .get_mut(&self.current_group)
                .unwrap()
                .add_child(tri);
        }
    }
}

impl From<ObjParser<'_>> for Group {
    fn from(p: ObjParser) -> Group {
        if p.groups.len() == 1 {
            p.groups.into_iter().map(|(_, g)| g).next().unwrap()
        } else {
            let mut group = Group::default();
            for g in p
                .groups
                .into_iter()
                .map(|(_, g)| g)
                .filter(Group::aint_empty)
            {
                group.add_child(g);
            }
            group
        }
    }
}

impl From<ObjParser<'_>> for SceneItem {
    fn from(p: ObjParser) -> SceneItem {
        SceneItem::Compound(p.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::approx_eq::ApproximateEq;
    use crate::shapes::smooth_triangle;
    use crate::tuple::{point, vector};

    /// Ignoring unrecognized lines
    #[test]
    fn parse_gibberish() {
        let gibberish = "There was a young lady named Bright
who traveled much faster than light.
She set out one day
in a relative way,
and came back the previous night.";
        let parser = ObjParser::parse_str(gibberish);
        assert_eq!(parser.ignored().len(), 5);
    }

    /// Vertex records
    #[test]
    fn parse_vertex_data() {
        let data = "\
            v -1 1 0
            v -1.0000 0.5000 0.0000
            v 1 0 0
            v 1 1 0";
        let parser = ObjParser::parse_str(data);
        assert_almost_eq!(
            parser.vertices,
            vec![
                point(-1, 1, 0),
                point(-1, 0.5, 0),
                point(1, 0, 0),
                point(1, 1, 0)
            ]
        );
    }

    /// Parse triangle faces
    #[test]
    fn parse_triangle_face() {
        let data = "\
            v -1 1 0
            v -1 0 0
            v 1 0 0
            v 1 1 0

            f 1 2 3
            f 1 3 4";
        let parser = ObjParser::parse_str(data);
        let g = parser.get_group("default_group");
        assert_almost_eq!(
            g.get_child(0).as_shape().unwrap(),
            triangle(parser.vertices[0], parser.vertices[1], parser.vertices[2])
        );
        assert_almost_eq!(
            g.get_child(1).as_shape().unwrap(),
            triangle(parser.vertices[0], parser.vertices[2], parser.vertices[3])
        );
    }

    /// Trianglulating polygons
    #[test]
    fn parse_poly_face() {
        let data = "\
            v -1 1 0
            v -1 0 0
            v 1 0 0
            v 1 1 0
            v 0 2 0

            f 1 2 3 4 5";
        let parser = ObjParser::parse_str(data);
        let g = parser.get_group("default_group");
        assert_almost_eq!(
            g.get_child(0).as_shape().unwrap(),
            triangle(parser.vertices[0], parser.vertices[1], parser.vertices[2])
        );
        assert_almost_eq!(
            g.get_child(1).as_shape().unwrap(),
            triangle(parser.vertices[0], parser.vertices[2], parser.vertices[3])
        );
        assert_almost_eq!(
            g.get_child(2).as_shape().unwrap(),
            triangle(parser.vertices[0], parser.vertices[3], parser.vertices[4])
        );
    }

    /// Triangles in groups
    #[test]
    fn parse_groups() {
        let parser = ObjParser::parse_str(DATA);
        let g1 = parser.get_group("FirstGroup");
        let g2 = parser.get_group("SecondGroup");
        assert_almost_eq!(
            g1.get_child(0).as_shape().unwrap(),
            triangle(parser.vertices[0], parser.vertices[1], parser.vertices[2])
        );
        assert_almost_eq!(
            g2.get_child(0).as_shape().unwrap(),
            triangle(parser.vertices[0], parser.vertices[2], parser.vertices[3])
        );
    }

    /// Converting an OBJ file to a group
    #[test]
    fn convert_to_group() {
        let parser = ObjParser::parse_str(DATA);
        let group: Group = parser.into();
        let g1 = group.get_child(0).as_group().unwrap();
        let g2 = group.get_child(1).as_group().unwrap();
        let parser = ObjParser::parse_str(DATA); // parse it again to get the vertex list :)
        assert_almost_eq!(
            g1.get_child(0).as_shape().unwrap(),
            triangle(parser.vertices[0], parser.vertices[1], parser.vertices[2])
        );
        assert_almost_eq!(
            g2.get_child(0).as_shape().unwrap(),
            triangle(parser.vertices[0], parser.vertices[2], parser.vertices[3])
        );
    }

    /// Vertex normal records
    #[test]
    fn vertex_normals() {
        let data = "\
            vn 0 0 1
            vn 0.707 0 -0.707
            vn 1 2 3";
        let parser = ObjParser::parse_str(data);
        assert_almost_eq!(parser.normals[0], vector(0, 0, 1));
        assert_almost_eq!(parser.normals[1], vector(0.707, 0, -0.707));
        assert_almost_eq!(parser.normals[2], vector(1, 2, 3));
    }

    /// Faces with normals
    #[test]
    fn faces_with_vertex_normals() {
        let data = "\
            v 0 1 0
            v -1 0 0
            v 1 0 0

            vn -1 0 0
            vn 1 0 0
            vn 0 1 0

            f 1//3 2//1 3//2
            f 1/0/3 2/102/1 3/14/2";
        let parser = ObjParser::parse_str(data);
        let g = parser.get_group("default_group");
        let t = smooth_triangle(
            parser.vertices[0],
            parser.vertices[1],
            parser.vertices[2],
            parser.normals[2],
            parser.normals[0],
            parser.normals[1],
        );
        assert_almost_eq!(g.get_child(0).as_shape().unwrap(), t);
        assert_almost_eq!(g.get_child(1).as_shape().unwrap(), t);
    }

    const DATA: &'static str = "\
        v -1 1 0
        v -1 0 0
        v 1 0 0
        v 1 1 0

        g FirstGroup
        f 1 2 3
        g SecondGroup
        f 1 3 4";
}
