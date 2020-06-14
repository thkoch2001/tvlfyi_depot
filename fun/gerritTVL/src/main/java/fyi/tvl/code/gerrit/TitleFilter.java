package fyi.tvl.code.gerrit;

import com.google.common.flogger.FluentLogger;
import com.google.gerrit.entities.Change;
import com.google.gerrit.extensions.restapi.ResourceConflictException;
import com.google.gerrit.extensions.restapi.ResourceNotFoundException;
import com.google.gerrit.httpd.AllRequestFilter;
import com.google.gerrit.server.change.ChangeResource;
import com.google.gerrit.server.permissions.PermissionBackendException;
import com.google.gerrit.server.restapi.change.ChangesCollection;
import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.Singleton;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Singleton
public final class TitleFilter extends AllRequestFilter {
  private final Provider<ChangesCollection> changes;

  private static final Pattern extractChangeIdRegex = Pattern.compile("^/(?:c/.*/\\+/)?(?<changeId>[0-9]+)(?:/[0-9]+)?$");
  private static final FluentLogger log = FluentLogger.forEnclosingClass();

  @Inject
  public TitleFilter(Provider<ChangesCollection> changes) {
    this.changes = changes;
  }

  private boolean isTitleBot(HttpServletRequest request) {
    String userAgent = request.getHeader("User-Agent");
    return userAgent.contains("like Basilisk");
  }

  private Optional<Change.Id> tryExtractChange(String path) {
    Matcher m = extractChangeIdRegex.matcher(path);
    if (!m.matches()) {
      return Optional.empty();
    }
    return Change.Id.tryParse(m.group("changeId"));
  }

  public boolean handle(ServletRequest request, ServletResponse response) {
    if (!(request instanceof HttpServletRequest)) {
      return false;
    }
    HttpServletRequest httpRequest = (HttpServletRequest) request;
    if (!isTitleBot(httpRequest)) {
      return false;
    }
    Optional<Change.Id> maybeChangeId = tryExtractChange(httpRequest.getRequestURI());
    if (!maybeChangeId.isPresent()) {
      return false;
    }

    ChangesCollection changesCollection = changes.get();
    ChangeResource changeResource;
    try {
      changeResource = changesCollection.parse(maybeChangeId.get());
    } catch (ResourceConflictException | ResourceNotFoundException | PermissionBackendException e) {
      return false;
    }

    try {
      response.setContentType("text/html");
      response.getWriter().print("<title>");
      response.getWriter().print(changeResource.getChange().getSubject());
      response.getWriter().print("</title>");
    } catch (IOException e) {
      log.atSevere().log("Failed to output change");
    }

    return true;
  }

  @Override
  public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
    if (!handle(request, response)) {
      chain.doFilter(request, response);
    }
  }

  @Override
  public void destroy() {
  }

  @Override
  public void init(FilterConfig filterConfig) {
  }
}
