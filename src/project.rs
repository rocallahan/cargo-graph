use std::collections::{HashMap, HashSet};

use toml::Value;

use dep::{DeclaredDep, DepKind};
use graph::DepGraph;
use error::{CliErrorKind, CliResult};
use config::Config;
use util;

#[derive(Debug)]
pub struct Project<'c, 'o>
    where 'o: 'c
{
    cfg: &'c Config<'o>,
}

impl<'c, 'o> Project<'c, 'o> {
    pub fn with_config(cfg: &'c Config<'o>) -> CliResult<Self> {
        Ok(Project { cfg: cfg })
    }

    pub fn graph(mut self) -> CliResult<DepGraph<'c, 'o>> {
        let root_path = try!(util::find_manifest_file(&self.cfg.manifest_file));
        let root_toml = try!(util::toml_from_file(root_path));
        if let Some(workspace) = root_toml.get("workspace") {
            if let Some(&Value::Array(ref members)) = pkg.lookup("members") {
                
            }
        }

        let (root_deps, root_name, root_version) = try!(self.parse_manifest_deps(&self.cfg.manifest_files[0]));
        let mut dep_hash = HashMap::new();
        dep_hash.insert((root_name.clone(), root_version.clone()), root_deps.clone());
        for m in self.cfg.manifest_files[1..].iter() {
            let (deps, name, version) = try!(self.parse_manifest_deps(m));
            dep_hash.insert((name, version), deps);
        }
        let mut dg = try!(self.parse_lock_file(&root_name, &root_version, &dep_hash));
        self.set_resolved_kind(&root_deps, &mut dg);
        if !self.cfg.include_vers {
            Project::show_version_on_duplicates(&mut dg);
        }
        Ok(dg)
    }

    /// Forces the version to be displayed on dependencies
    /// that have the same name (but a different version) as another dependency.
    fn show_version_on_duplicates(dg: &mut DepGraph<'c, 'o>) {
        // Build a list of node IDs, sorted by the name of the dependency on that node.
        let dep_ids_sorted_by_name = {
            let mut deps = dg.nodes.iter().enumerate().collect::<Vec<_>>();
            deps.sort_by_key(|dep| &*dep.1.name);
            deps.iter().map(|dep| dep.0).collect::<Vec<_>>()
        };

        for (i, &dep_id_i) in dep_ids_sorted_by_name.iter().enumerate().take(dep_ids_sorted_by_name.len() - 1) {
            // Find other nodes with the same name
            // We need to iterate one more time after the last node to handle the break.
            for (j, &dep) in dep_ids_sorted_by_name.iter().enumerate().take(dep_ids_sorted_by_name.len() + 1).skip(i + 1) {
                // Stop once we've found a node with a different name
                // or reached the end of the list.
                if j >= dep_ids_sorted_by_name.len() ||
                   dg.nodes[dep_id_i].name != dg.nodes[dep].name {
                    // If there are at least two nodes with the same name
                    if j >= i + 2 {
                        // Set force_write_ver = true on all nodes
                        // from dep_ids_sorted_by_name[i] to dep_ids_sorted_by_name[j - 1].
                        // Remember: j is pointing on the next node with a *different* name!
                        // Remember also: i..j includes i but excludes j.
						for &dep_id_k in dep_ids_sorted_by_name.iter().take(j).skip(i) {
                            dg.nodes[dep_id_k].force_write_ver = true;
                        }
                    }

                    break;
                }
            }
        }
    }

    /// Sets the kind of dependency on each dependency
    /// based on how the dependencies are declared in the manifest.
    fn set_resolved_kind(&mut self, declared_deps: &[DeclaredDep], dg: &mut DepGraph<'c, 'o>) {
        let declared_deps_map =
            declared_deps.iter().map(|dd| (&*dd.name, dd.kind)).collect::<HashMap<_, _>>();

        dg.nodes[0].is_build = true;

        dg.edges.sort(); // make sure to process edges from the root node first
        for ed in dg.edges.iter() {
            if ed.0 == 0 {
                // If this is an edge from the root node,
                // set the kind based on how the dependency is declared in the manifest file.
                if let Some(kind) = declared_deps_map.get(&*dg.nodes[ed.1].name) {
                    match *kind {
                        DepKind::Build => dg.nodes[ed.1].is_build = true,
                        DepKind::Dev => dg.nodes[ed.1].is_dev = true,
                        DepKind::Optional => dg.nodes[ed.1].is_optional = true,
                        _ => (),
                    }
                }
            } else {
                // If this is an edge from a dependency node, propagate the kind.
                // This is a set of flags because a dependency can appear several times in the graph,
                // and the kind of dependency may vary based on the path to that dependency.
                // The flags start at false, and once they become true, they stay true.
                // ResolvedDep::kind() will pick a kind based on their priority.
                if dg.nodes[ed.0].is_build {
                    dg.nodes[ed.1].is_build = true;
                }

                if dg.nodes[ed.0].is_dev {
                    dg.nodes[ed.1].is_dev = true;
                }

                if dg.nodes[ed.0].is_optional {
                    dg.nodes[ed.1].is_optional = true;
                }
            }
        }

        // Remove the nodes that the user doesn't want.
        // Start at 1 to keep the root node.
        for id in (1..dg.nodes.len()).rev() {
            let kind = dg.nodes[id].kind();
            if (kind == DepKind::Build && !self.cfg.build_deps) ||
               (kind == DepKind::Dev && !self.cfg.dev_deps) ||
               (kind == DepKind::Optional && !self.cfg.optional_deps) {
                dg.remove(id);
            }
        }

        dg.remove_orphans();
    }

    /// Builds a graph of the resolved dependencies declared in the lock file.
    fn parse_lock_file(&mut self,
                       root_name: &str,
                       root_version: &str,
                       dep_hash: &HashMap<(String, String), Vec<DeclaredDep>>)
                       -> CliResult<DepGraph<'c, 'o>> {
        fn parse_package<'c, 'o>(dg: &mut DepGraph<'c, 'o>, pkg: &Value,
                                 dep_hash: &HashMap<(String, String), Vec<DeclaredDep>>) {
            let name = pkg.lookup("name")
                .expect("no 'name' field in Cargo.lock [package] or [root] table")
                .as_str()
                .expect("'name' field of [package] or [root] table in Cargo.lock was not a \
                                   valid string")
                .to_owned();
            let ver = pkg.lookup("version")
                .expect("no 'version' field in Cargo.lock [package] or [root] table")
                .as_str()
                .expect("'version' field of [package] or [root] table in Cargo.lock was not a \
                                  valid string")
                .to_owned();

            let id = dg.find_or_add(&*name, &*ver);

            if let Some(&Value::Array(ref deps)) = pkg.lookup("dependencies") {
               let deps_filter = dep_hash.get(&(name, ver)).map(|deps| {
                   let mut set = HashSet::new();
                   for dep in deps {
                       match dep.kind {
                           DepKind::Build | DepKind::Unk => { set.insert(&dep.name); },
                           _ => ()
                       };
                   }
                   set
               });
               for dep in deps {
                    let dep_vec = dep.as_str().unwrap_or("").split(' ').collect::<Vec<_>>();
                    let dep_string = dep_vec[0].to_owned();
                    let ver = dep_vec[1];
                    if deps_filter.as_ref().map(|deps_set| deps_set.contains(&dep_string)).unwrap_or(true) {
                        dg.add_child(id, &*dep_string, ver);
                    }
                }
            }
        }

        let lock_path = try!(util::find_manifest_file(self.cfg.lock_file));
        let lock_toml = try!(util::toml_from_file(lock_path));

        let mut dg = DepGraph::new(self.cfg);

        // Use the specified package as the root
        dg.find_or_add(root_name, root_version);

        if let Some(root) = lock_toml.get("root") {
            parse_package(&mut dg, root, dep_hash);
        } else {
            return Err(From::from(CliErrorKind::TomlTableRoot));
        }

        if let Some(&Value::Array(ref packages)) = lock_toml.get("package") {
            for pkg in packages {
                parse_package(&mut dg, pkg, dep_hash);
            }
        }

        debugln!("return=parse_lock_file; self={:#?}", self);
        Ok(dg)
    }

    /// Builds a list of the dependencies declared in the manifest file.
    pub fn parse_manifest_deps(&mut self, manifest: &str) -> CliResult<(String, String, Vec<DeclaredDep>)> {
        debugln!("executing; parse_manifest_deps;");
        let manifest_path = try!(util::find_manifest_file(manifest));
        let manifest_toml = try!(util::toml_from_file(manifest_path));

        let mut declared_deps = vec![];
        let mut v = vec![];

        let package = manifest_toml.get("package")
            .expect("no [package] table in Cargo.toml");
        let name = package.lookup("name")
            .expect("no 'name' field in Cargo.toml [package] table")
            .as_str()
            .expect("'name' field of [package] table in Cargo.toml was not a \
                                   valid string")
            .to_owned();
        let version = package.lookup("version")
            .expect("no 'version' field in Cargo.toml [package] table")
            .as_str()
            .expect("'version' field of [package] table in Cargo.toml was not a \
                                   valid string")
            .to_owned();

        if let Some(table) = manifest_toml.get("dependencies") {
            if let Some(table) = table.as_table() {
                for (name, dep_table) in table.into_iter() {
                    if let Some(&Value::Boolean(true)) = dep_table.lookup("optional") {
                        declared_deps.push(DeclaredDep::with_kind(name.clone(), DepKind::Optional));
                    } else {
                        declared_deps.push(DeclaredDep::with_kind(name.clone(), DepKind::Build));
                    }
                    v.push(name.clone());
                }
            }
        }

        if let Some(table) = manifest_toml.get("dev-dependencies") {
            if let Some(table) = table.as_table() {
                for (name, _) in table.into_iter() {
                    declared_deps.push(DeclaredDep::with_kind(name.clone(), DepKind::Dev));
                    v.push(name.clone());
                }
            }
        }

        debugln!("return=parse_manifest_deps; self={:#?}", self);
        debugln!("return=parse_manifest_deps; declared_deps={:#?}", declared_deps);
        Ok((name, version, declared_deps))
    }
}
