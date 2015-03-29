library(whisker)

redirects <- c(Philosophy.html = "intro.html", `Package-basics.html` = "description.html", 
    `Package-development-cycle.html` = "r.html", `Package-quick-reference.html` = "r.html", 
    `Documenting-packages.html` = "vignettes.html", `Documenting-functions.html` = "man.html", 
    Namespaces.html = "namespace.html", Style.html = "style.html", 
    Testing.html = "tests.html", Git.html = "git.html", Release.html = "release.html")

data <- list(rule = lapply(seq_along(redirects), function(i) list(old = names(redirects)[i], 
    new = redirects[[i]])))

template <- "\n<RoutingRules>\n{{#rule}}\n  <RoutingRule>\n    <Condition>\n      <KeyPrefixEquals>{{{old}}}</KeyPrefixEquals>\n    </Condition>\n    <Redirect>\n      <HostName>r-pkgs.had.co.nz</HostName>\n      <ReplaceKeyWith>{{{new}}}</ReplaceKeyWith>\n    </Redirect>\n  </RoutingRule>\n{{/rule}}\n</RoutingRules>\n"

cat(whisker.render(template, data)) 
