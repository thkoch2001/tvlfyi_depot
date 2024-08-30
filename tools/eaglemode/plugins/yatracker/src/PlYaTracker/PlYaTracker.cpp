#include <emCore/emFilePanel.h>
#include <emCore/emFpPlugin.h>
#include <emCore/emRecFileModel.h>
#include <emCore/emToolkit.h>

class PlYaTrackerConfig final : public emRecFileModel, public emStructRec {
 public:
  static emRef<PlYaTrackerConfig> Acquire(emContext& context,
                                          const emString& name,
                                          bool common = true);

  virtual const char* GetFormatName() const;

  emStringRec URL;
  emStringRec Token;

 protected:
  PlYaTrackerConfig(emContext& context, const emString& name);
};

emRef<PlYaTrackerConfig> PlYaTrackerConfig::Acquire(emContext& context,
                                                    const emString& name,
                                                    bool common) {
  EM_IMPL_ACQUIRE(PlYaTrackerConfig, context, name, common)
}

const char* PlYaTrackerConfig::GetFormatName() const { return "PlYaTracker"; }

PlYaTrackerConfig::PlYaTrackerConfig(emContext& context, const emString& name)
    : emRecFileModel(context, name),
      emStructRec(),
      URL(this, "URL"),
      Token(this, "Token") {
  PostConstruct(*this);
}

class PlYaTrackerFilePanel : public emFilePanel {
 public:
  PlYaTrackerFilePanel(ParentArg parent, const emString& name,
                       emRef<PlYaTrackerConfig> config);

 private:
  emRef<PlYaTrackerConfig> Config;
};

PlYaTrackerFilePanel::PlYaTrackerFilePanel(ParentArg parent,
                                           const emString& name,
                                           emRef<PlYaTrackerConfig> config)
    : emFilePanel(parent, name, config), Config(config) {}

extern "C" {
emPanel* PlYaTrackerPluginFunc(emPanel::ParentArg parent, const emString& name,
                               const emString& path, emFpPlugin* plugin,
                               emString* errorBuf) {
  return new PlYaTrackerFilePanel(
      parent, name, PlYaTrackerConfig::Acquire(parent.GetRootContext(), path));
}
}
