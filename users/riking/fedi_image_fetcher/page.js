(function () {
  const rootEl = document.getElementById('app');
  const vueRoot = rootEl.__vue__;

  function childWithTag(vueEl, tagName) {
    return vueEl.$children.find(child => child.$vnode.componentOptions.tag === tagName);
  }

  function escapeRegExp(string) {
    return string.replace(/[.*+\-?^${}()|[\]\\]/g, '\\$&'); // $& means the whole matched string
  }

  function downloadNamePrefix(panelBodyVue) {
    const usernameSplit = panelBodyVue.status.user.screen_name.split('@');
    const DOMAIN = 1, LOCAL_PART = 0;

    let urlIdPart = "";
    if (panelBodyVue.status.external_url) {
      const parsedUrl = new URL(panelBodyVue.status.external_url);
      let pathname = parsedUrl.pathname;
      pathname = pathname.replace(new RegExp("@?" + escapeRegExp(usernameSplit[LOCAL_PART]), 'g'), '');
      pathname = pathname.replace(/(users|statuses|notice|notes|objects)/g, '');
      pathname = pathname.replace(/\/+/g, '-');
      pathname = pathname.replace(/(^-|-$)/g, '');
      urlIdPart = "-" + pathname;
    }

    return usernameSplit[DOMAIN] + "-" + usernameSplit[LOCAL_PART] + urlIdPart;
  }

  function downloadMedia(clickEl) {
    let statusRoot = clickEl;
    while (statusRoot && !statusRoot.classList.contains('panel-body')) {
      statusRoot = statusRoot.parentElement;
    }
    if (!statusRoot) {
      console.error("could not find panel-body parent for click", clickEl);
      return;
    }
    statusRoot = statusRoot.parentElement;
    const statusVue = statusRoot.__vue__;
    const panelBodyVue = statusVue.$children[0];
    const statusContentVue = childWithTag(panelBodyVue, "StatusContent");
    const galleryVue = childWithTag(statusContentVue, "gallery");

    if (galleryVue) {
      const prefix = downloadNamePrefix(panelBodyVue); 

      galleryVue.attachments.forEach(atch => {
	var link = document.createElement('a');
	link.href = atch.url;
	link.download = prefix + "-" + atch.id;
	document.body.appendChild(link);
	link.click();
	document.body.removeChild(link);
      });
    }
  }

  function hookPostAdd() {
  }

  hookPostAdd();
)();
