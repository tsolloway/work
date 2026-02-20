/**
 * sync-config.js
 *
 * Reads launcher.config.json and updates package.json so that
 * electron-builder picks up the correct app name, file extension,
 * and icon at build time. Run automatically via the "prebuild" script.
 */
const fs = require("fs");
const path = require("path");

const configPath = path.join(__dirname, "launcher.config.json");
const pkgPath = path.join(__dirname, "package.json");

const config = JSON.parse(fs.readFileSync(configPath, "utf-8"));
const pkg = JSON.parse(fs.readFileSync(pkgPath, "utf-8"));

const ext = config.fileExtension || "resondex";

// ---- Update package name and build identifiers (unique per launcher) ----
const slug = config.appName.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
pkg.name = slug; // Electron uses this for the userData directory path
pkg.version = config.version || "1.0.0";
pkg.build.appId = `com.shiny-launcher.${slug}`;
pkg.build.productName = config.appName;
pkg.build.copyright = `Copyright \u00A9 ${new Date().getFullYear()} Resondex`;

// ---- Update build.fileAssociations ----
pkg.build.fileAssociations = [
  {
    ext: ext,
    name: `${config.appName} Bundle`,
    description: `Application bundle for ${config.appName}`,
    mimeType: `application/x-${ext}`,
    role: "Viewer",
  },
];

// ---- Update build icon if configured ----
pkg.build.mac = pkg.build.mac || {};
pkg.build.win = pkg.build.win || {};
if (config.icon) {
  pkg.build.mac.icon = config.icon;
  pkg.build.win.icon = config.icon;
} else {
  // Explicitly remove icon keys so electron-builder doesn't choke on
  // undefined/null values during schema validation
  delete pkg.build.mac.icon;
  delete pkg.build.win.icon;
}

// Disable macOS code signing (set identity to null in config)
pkg.build.mac.identity = null;

// ---- Include launcher.config.json in the build ----
if (!pkg.build.files.includes("launcher.config.json")) {
  pkg.build.files.push("launcher.config.json");
}

fs.writeFileSync(pkgPath, JSON.stringify(pkg, null, 2) + "\n");
console.log(`[sync-config] Updated package.json — ext: .${ext}, productName: "${pkg.build.productName}"`);
