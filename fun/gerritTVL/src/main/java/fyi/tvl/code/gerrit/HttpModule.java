package fyi.tvl.code.gerrit;

import com.google.common.flogger.FluentLogger;
import com.google.gerrit.extensions.registration.DynamicSet;
import com.google.gerrit.httpd.AllRequestFilter;
import com.google.inject.servlet.ServletModule;

public final class HttpModule extends ServletModule {
  private static final FluentLogger log = FluentLogger.forEnclosingClass();

  @Override
  protected void configureServlets() {
    // Filter paths to CLs through the TitleFilter.
    DynamicSet.bind(binder(), AllRequestFilter.class).to(TitleFilter.class);
  }
}
