package config

import java.nio.file.Paths

import javax.inject.Singleton
import org.eclipse.jgit.api.Git


@Singleton
class RepoRoots {
  val sheets =Git.open(Paths.get("internal/test/sheets").toFile)
  val grants =Git.open(Paths.get("internal/test/grants").toFile)
}
