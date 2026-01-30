use std::collections::HashSet;
use std::path::Path;

use calyx_frontend::{LibrarySignatures, Workspace, parser::CalyxParser};
use calyx_utils::{CalyxResult, Error};

use super::{Import, ImportPaths};

pub fn build_library(
    search_paths: &[&Path],
) -> CalyxResult<(LibrarySignatures, ImportPaths)> {
    let mut stack: Vec<_> = Import::PATHS
        .iter()
        .map(|&import| {
            search_paths
                .iter()
                .find_map(|&lib_path| {
                    let import = lib_path.join(import).canonicalize().ok()?;

                    import.exists().then_some((import, lib_path))
                })
                .ok_or_else(|| {
                    Error::invalid_file(format!("Unresolved import `{import}`"))
                })
        })
        .collect::<CalyxResult<_>>()?;

    let paths = ImportPaths::new(|i| stack[i].0.to_string_lossy().into_owned());

    let mut workspace = Workspace::default();
    let mut imported = HashSet::new();

    while let Some((import, lib_path)) = stack.pop() {
        if imported.contains(&import) {
            continue;
        }

        let namespace = CalyxParser::parse_file(&import)?;
        let parent = import.parent().unwrap();

        let next = workspace
            .merge_namespace(namespace, false, parent, true, lib_path)?;

        stack.extend(next.into_iter().map(|(import, _)| (import, lib_path)));
        imported.insert(import);
    }

    Ok((workspace.lib, paths))
}
