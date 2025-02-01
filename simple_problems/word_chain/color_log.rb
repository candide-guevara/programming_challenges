require "logger"

ColReset = "\e[0m"

ColMap = {
  "DEBUG" => "\e[0;36m",
  "INFO" => "\e[0;32m",
  "WARN" => "\e[1;33m",
  "ERROR" => "\e[1;31m",
  "FATAL" => "\e[41m",
}

$my_formater = lambda do |severity, datetime, progname, msg|
  "#{ColMap[severity]}#{datetime.strftime("%y%m%d-%T")} [#{severity}]#{ColReset} #{msg}\n"
end
  
def initLogging (name, formatter: $my_formater, output: STDOUT)
    logger = Logger.new output
    logger.progname = File.basename(name, '.rb')
    logger.formatter = formatter
    logger.level = Logger::INFO
    return logger
end

