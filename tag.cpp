#include <array>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <optional>
#include <regex>
#include <sstream>
#include <unistd.h>
#include <utility>

#define let const auto

static const std::string QF_FILE("/tmp/tag_qf");
static const std::regex FILE_REGEX("^(?:(?:\x1b\\[[^m]*m)+)?([^\x1b]+)");
static const std::regex
    LINE_NUMBER_MATCH("^(?:(?:\x1b\\[[^m]*m)+)?(\\d+)(?:(?:\x1b\\[0?m(?:\x1b\\["
                      "K)?)+)?:(?:\x1b\\[0?m)?(\\d+)(?:\x1b\\[0?m)?:(.*)");
const std::regex ANSI_REGEX("\x1b\\[[0-9;]*[mG]");

class Command {
  const std::string cmd_id_;
  const std::string cmd_;
  const std::string extraArgs_ = "";
  const bool includeFiles_ = false;
  [[nodiscard]] std::string command(const std::string &args) const {
    return cmd_ + " " + extraArgs_ + " " + args;
  }

  [[nodiscard]] std::string
  commandWithoutDefaultArgs(const std::string &args) const {
    return cmd_ + " " + args;
  }

public:
  Command(std::string cmd, const char *extraArgs)
      : cmd_id_(cmd), cmd_(cmd), extraArgs_(extraArgs) {}

  Command(std::string cmd, bool includeFiles)
      : cmd_id_(cmd), cmd_(cmd), includeFiles_(includeFiles) {}

  Command(std::string cmd_id, std::string cmd, bool includeFiles)
      : cmd_id_(cmd_id), cmd_(cmd), includeFiles_(includeFiles) {}

  std::string cmd_id() const { return cmd_id_; }
  bool includeFiles() const { return includeFiles_; }

  [[nodiscard]] int
  run(const std::string &args,
      std::function<void(const std::string &)> callback) const {
    FILE *pipe = popen(this->command(args).c_str(), "r");
    if (!pipe) {
      std::cerr << "error: failed to launch process" << std::endl;
      return 1;
    }

    char *line = nullptr;
    size_t _len = 0;

    while (getline(&line, &_len, pipe) != -1)
      callback(line);
    free(line);

    int status = pclose(pipe);
    if (WIFEXITED(status))
      return WEXITSTATUS(status);
    if (WIFSIGNALED(status))
      return WTERMSIG(status);

    return status;
  }

  [[nodiscard]] int runWithoutDefaultArgs(const std::string &args) const {
    int status = std::system(this->commandWithoutDefaultArgs(args).c_str());
    if (WIFEXITED(status))
      return WEXITSTATUS(status);

    return status;
  }
};

static const std::array COMMANDS{
    Command("rg", "--heading --color always --column"),
    Command("ag", "--group --color --column"),
    Command("fd", true),
    Command("fdfind", true),
    Command("find", true),
    Command("ls", true),
    Command("git-status", "git -c color.status=always status -sb", true),
};

[[nodiscard]] static bool isZSH() {
  if (let *shell = std::getenv("SHELL"))
    if (std::string(shell).find("zsh") != std::string::npos)
      return true;

  return false;
}

static inline void rstrip(std::string &s) {
  s.erase(std::find_if(s.rbegin(), s.rend(),
                       [](unsigned char ch) { return !std::isspace(ch); })
              .base(),
          s.end());
}

static inline void stripLsClassifier(std::string &line) {
  if (line.find_last_of("*@=%|") == line.size() - 1) {
    line.pop_back();
  }
}

// FIXME: cleanup somehow
[[nodiscard]] static std::string
vimEditCommand(const std::string &path,
               std::optional<std::pair<int, int>> location,
               const std::string &vimArgs = "") {
  const std::string prefix =
      "eval '$EDITOR " + vimArgs + "\\\"" + path + "\\\"";
  if (location) {
    let line = std::to_string(std::get<0>(*location));
    let col = std::to_string(std::get<1>(*location));
    return prefix + " \\\"+call cursor(" + line + ", " + col + ")\\\"'";
  }

  return prefix + "'";
}

[[nodiscard]] static std::string aliasForCommand(const std::string index,
                                                 const std::string &command) {
  return "alias e" + index + "=\"" + command + "\"";
}

[[nodiscard]] static std::string aliasForCommand(int index,
                                                 const std::string &command) {
  return aliasForCommand(std::to_string(index), command);
}

[[nodiscard]] static std::string
globalAliasForCommand(int index, const std::string &command) {
  return "alias -g f" + std::to_string(index) + "=\"" + command + "\"";
}

static void printAliasedLine(int index, std::string line) {
  std::cout << "[\x1b[0;31m" << index << "\x1b[0m] " << line;
}

[[nodiscard]] static std::string stripAnsi(const std::string line) {
  std::stringstream result;
  std::regex_replace(std::ostream_iterator<char>(result), line.begin(),
                     line.end(), ANSI_REGEX, "");

  return result.str();
}

[[nodiscard]] static std::string formatQFLine(const std::string &path, int lnum,
                                              int col,
                                              const std::string line = "") {
  return path + ":" + std::to_string(lnum) + ":" + std::to_string(col) + ":" +
         line;
}

[[nodiscard]] static int
runAndWriteFile(const Command cmd, std::string outputFile, std::string args) {
  std::string currentFile;
  int aliasIndex = 1;
  std::ofstream os(outputFile);
  std::ofstream qfOS(QF_FILE);

  os << aliasForCommand("all", vimEditCommand(QF_FILE, std::nullopt, "-q "))
     << std::endl;

  return cmd.run(args, [&](const std::string &line) {
    std::smatch match;
    if (std::regex_search(line, match, LINE_NUMBER_MATCH)) {
      // TODO: https://github.com/keith/tag/issues/24
      if (!currentFile.size()) {
        std::cout << line;
        return;
      }

      let lnum = std::stoi(match.str(1));
      let col = std::stoi(match.str(2));
      let leftover = stripAnsi(match.str(3));

      let editCmd = vimEditCommand(currentFile, std::make_pair(lnum, col));
      os << aliasForCommand(aliasIndex, editCmd) << std::endl;
      if (isZSH()) {
        os << globalAliasForCommand(aliasIndex, currentFile) << std::endl;
      }

      qfOS << formatQFLine(currentFile, lnum, col, leftover) << std::endl;

      printAliasedLine(aliasIndex, line);
      ++aliasIndex;
    } else if (cmd.cmd_id() == "git-status") {
      if (line.starts_with("##")) {
        std::cout << line;
        return;
      }

      // TODO: Doesn't handle renames or moves
      std::string path = stripAnsi(line).substr(3);
      rstrip(path);

      qfOS << formatQFLine(path, 1, 1, "") << std::endl;
      printAliasedLine(aliasIndex, line);

      let editCmd = vimEditCommand(path, std::nullopt);
      os << aliasForCommand(aliasIndex, editCmd) << std::endl;
      if (isZSH()) {
        os << globalAliasForCommand(aliasIndex, path) << std::endl;
      }

      ++aliasIndex;
    } else if (cmd.cmd_id() == "ls") {
      if (aliasIndex == 1 && line.starts_with("total")) {
        // ls -l shows a total first
        // probably broken if there's only 1 file called "total"
        std::cout << line;
        return;
      }

      std::string file = stripAnsi(line);
      rstrip(file);
      stripLsClassifier(file);

      file = file.substr(file.find_last_of(' ') + 1);

      qfOS << formatQFLine(file, 1, 1, "") << std::endl;
      printAliasedLine(aliasIndex, line);

      let editCmd = vimEditCommand(file, std::nullopt);
      os << aliasForCommand(aliasIndex, editCmd) << std::endl;
      if (isZSH()) {
        os << globalAliasForCommand(aliasIndex, file) << std::endl;
      }

      ++aliasIndex;
    } else if (std::regex_search(line, match, FILE_REGEX)) {
      currentFile = match.str(1);
      rstrip(currentFile);
      if (cmd.includeFiles()) {
        qfOS << formatQFLine(currentFile, 1, 1, "") << std::endl;
        printAliasedLine(aliasIndex, line);

        let editCmd = vimEditCommand(currentFile, std::nullopt);
        os << aliasForCommand(aliasIndex, editCmd) << std::endl;
        if (isZSH()) {
          os << globalAliasForCommand(aliasIndex, currentFile) << std::endl;
        }

        ++aliasIndex;
      } else {
        std::cout << line;
      }
    } else {
      std::cout << line;
    }

    // Force write in case of ctrl-c
    os.flush();
  });
}

[[noreturn]] static void helpAndExit() {
  std::stringstream cmdNames;
  cmdNames << "[";
  for (auto cmd : COMMANDS)
    cmdNames << cmd.cmd_id() << "|";

  cmdNames.seekp(-1, cmdNames.cur);
  cmdNames << ']';

  std::cerr << "Usage: tag [--alias-file FILE] " << cmdNames.str() << " ARGS"
            << std::endl;
  std::exit(127);
}

[[nodiscard]] static std::string popFirst(std::deque<std::string> &queue) {
  if (queue.empty())
    helpAndExit();

  auto element = queue.front();
  queue.pop_front();
  return element;
}

int main(int argc, char *argv[]) {
  std::deque<std::string> arguments(argv + 1, argv + argc);
  if (arguments.empty())
    helpAndExit();

  std::string argFile("/tmp/tag_aliases");
  if (arguments.front() == "--alias-file") {
    arguments.pop_front();
    argFile = popFirst(arguments);
  }

  let cmdString = popFirst(arguments);
  let *command =
      std::find_if(COMMANDS.begin(), COMMANDS.end(), [&cmdString](auto &cmd) {
        return cmdString == cmd.cmd_id();
      });

  if (command == COMMANDS.end())
    helpAndExit();

  std::stringstream joinedArgs;
  for (auto arg : arguments) {
    joinedArgs << std::quoted(arg) << " ";
  }

  let args = joinedArgs.str();
  let isPiped = !isatty(fileno(stdout));
  if (isPiped && std::getenv("SKIP_PIPE_FILTERING") == nullptr)
    return command->runWithoutDefaultArgs(args);

  return runAndWriteFile(*command, argFile, args);
}
