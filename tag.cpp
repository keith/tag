#include <array>
#include <fstream>
#include <iostream>
#include <optional>
#include <regex>
#include <sstream>
#include <unistd.h>
#include <utility>
#include <vector>

#define let const auto

static const std::regex FILE_REGEX("^(?:(?:\x1b\\[[^m]*m)+)?([^\x1b]+)");
static const std::regex
    LINE_NUMBER_MATCH("^(?:(?:\x1b\\[[^m]*m)+)?(\\d+)(?:(?:\x1b\\[0?m(?:\x1b\\["
                      "K)?)+)?:(?:\x1b\\[0?m)?(\\d+)(?:\x1b\\[0?m)?:");

class Command {
  const std::string cmd_;
  const std::string extraArgs_ = "";
  const bool includeFiles_ = false;
  [[nodiscard]] std::string command(const std::string &args) const {
    return cmd_ + " " + extraArgs_ + " " + args;
  }

  [[nodiscard]] std::string commandWithoutDefaultArgs(const std::string &args) const {
    return cmd_ + " " + args;
  }

public:
  Command(std::string cmd, const char *extraArgs)
      : cmd_(cmd), extraArgs_(extraArgs) {}

  Command(std::string cmd, bool includeFiles)
      : cmd_(cmd), includeFiles_(includeFiles) {}

  std::string cmd() const { return cmd_; }
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

    return pclose(pipe);
  }

  [[nodiscard]] int runWithoutDefaultArgs(const std::string &args) const {
    return std::system(this->commandWithoutDefaultArgs(args).c_str());
  }
};

static const std::array COMMANDS{
    Command("rg", "--heading --color always --column"),
    Command("ag", "--group --color --column"),
    Command("fd", true),
    Command("fdfind", true),
    Command("find", true),
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

// FIXME: cleanup somehow
[[nodiscard]] static std::string
vimEditCommand(const std::string &path,
               std::optional<std::pair<int, int>> location) {
  const std::string prefix = "eval '$EDITOR \\\"" + path + "\\\"";
  if (location) {
    let line = std::to_string(std::get<0>(*location));
    let col = std::to_string(std::get<1>(*location));
    return prefix + " \\\"+call cursor(" + line + ", " + col + ")\\\"'";
  }

  return prefix + "'";
}

[[nodiscard]] static std::string aliasForCommand(int index,
                                                 const std::string &command) {
  return "alias e" + std::to_string(index) + "=\"" + command + "\"";
}

[[nodiscard]] static std::string
globalAliasForCommand(int index, const std::string &command) {
  return "alias -g f" + std::to_string(index) + "=\"" + command + "\"";
}

static void printAliasedLine(int index, std::string line) {
  std::cout << "[\x1b[0;31m" << index << "\x1b[0m] " << line;
}

[[nodiscard]] static int
runAndWriteFile(const Command cmd, std::string outputFile, std::string args) {
  std::string currentFile;
  int aliasIndex = 1;
  std::ofstream os(outputFile);

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

      let editCmd = vimEditCommand(currentFile, std::make_pair(lnum, col));
      os << aliasForCommand(aliasIndex, editCmd) << std::endl;
      if (isZSH()) {
        os << globalAliasForCommand(aliasIndex, currentFile) << std::endl;
      }

      printAliasedLine(aliasIndex, line);
      ++aliasIndex;
    } else if (std::regex_search(line, match, FILE_REGEX)) {
      currentFile = match.str(1);
      rstrip(currentFile);
      if (cmd.includeFiles()) {
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
    cmdNames << cmd.cmd() << "|";

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
      std::find_if(COMMANDS.begin(), COMMANDS.end(),
                   [&cmdString](auto &cmd) { return cmdString == cmd.cmd(); });

  if (command == COMMANDS.end())
    helpAndExit();

  std::stringstream joinedArgs;
  for (auto arg : arguments)
    joinedArgs << arg << " ";

  let args = joinedArgs.str();
  let isPiped = !isatty(fileno(stdout));
  if (isPiped && std::getenv("SKIP_PIPE_FILTERING") == nullptr)
    return command->runWithoutDefaultArgs(args);

  return runAndWriteFile(*command, argFile, args);
}
