import winston from "winston";

const { format } = winston;
const {
  combine,
  timestamp,
  prettyPrint,
  label,
  simple,
  printf,
  json,
  colorize,
} = winston.format;

export const logger = winston.createLogger({
  level: "info",
  format: combine(timestamp(), json(), prettyPrint(), colorize()),
  // defaultMeta: { service: "user-service" },
  transports: [
    // Write all logs with level `info` and below to `combined.log`
    // Write all logs with level `error` and below to `error.log`
    new winston.transports.File({ filename: "error.log", level: "error" }),
    new winston.transports.File({ filename: "combined.log" }),
  ],
});

// If we're not in production then log to the `console` with the format:
// `${info.level}: ${info.message} JSON.stringify({ ...rest }) `
if (process.env.NODE_ENV !== "production") {
  logger.add(
    new winston.transports.Console({
      format: combine(simple(), timestamp()),
    })
  );
}
