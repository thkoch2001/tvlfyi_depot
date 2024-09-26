#include <emCore/emFpPlugin.h>
#include <emCore/emImageFile.h>

#include "avif/avif.h"

class PlAvifImageFileModel : public emImageFileModel
{
public:

	static emRef<PlAvifImageFileModel> Acquire(
		emContext & context, const emString & name, bool common=true
	);

protected:
	PlAvifImageFileModel(emContext & context, const emString & name);
	virtual ~PlAvifImageFileModel();
	virtual void TryStartLoading();
	virtual bool TryContinueLoading();
	virtual void QuitLoading();
	virtual void TryStartSaving();
	virtual bool TryContinueSaving();
	virtual void QuitSaving();
	virtual emUInt64 CalcMemoryNeed();
	virtual double CalcFileProgress();

private:
	struct LoadingState;
	LoadingState * L = NULL;
};


struct PlAvifImageFileModel::LoadingState {
	avifRGBImage rgb;
	avifDecoder * decoder;
};


emRef<PlAvifImageFileModel> PlAvifImageFileModel::Acquire(
	emContext & context, const emString & name, bool common
)
{
	EM_IMPL_ACQUIRE(PlAvifImageFileModel, context, name, common)
}


PlAvifImageFileModel::PlAvifImageFileModel(
	emContext & context, const emString & name
)
	: emImageFileModel(context, name)
{
}


PlAvifImageFileModel::~PlAvifImageFileModel()
{
	PlAvifImageFileModel::QuitLoading();
	PlAvifImageFileModel::QuitSaving();
}


void PlAvifImageFileModel::TryStartLoading()
{
	avifResult result;

	L = new LoadingState;
	memset(L, 0, sizeof(LoadingState));

	L->decoder = avifDecoderCreate();
	if (L->decoder == NULL) {
		throw emException("failed to create AVIF decoder");
	}

	result = avifDecoderSetIOFile(L->decoder, GetFilePath());
	if (result != AVIF_RESULT_OK) {
		throw emException("%s", avifResultToString(result));
	}

	result = avifDecoderParse(L->decoder);
	if (result != AVIF_RESULT_OK) {
		throw emException("%s", avifResultToString(result));
	}

	FileFormatInfo = emString::Format(
		"AVIF %s %ubpc",
			avifPixelFormatToString(L->decoder->image->yuvFormat),
			L->decoder->image->depth
	);


	Signal(ChangeSignal);
}


bool PlAvifImageFileModel::TryContinueLoading()
{
	avifResult result;

	if (!Image.GetHeight()) {
		Image.Setup(
			L->decoder->image->width,
			L->decoder->image->height,
			L->decoder->alphaPresent ? 4 : 3
		);
	}

	result = avifDecoderNextImage(L->decoder);
	if (result != AVIF_RESULT_OK) {
		throw emException("%s", avifResultToString(result));
	}

	avifRGBImageSetDefaults(&L->rgb, L->decoder->image);
	L->rgb.format = L->decoder->alphaPresent ?
		AVIF_RGB_FORMAT_RGBA : AVIF_RGB_FORMAT_RGB;
	L->rgb.pixels   = Image.GetWritableMap();
	L->rgb.width    = Image.GetWidth();
	L->rgb.height   = Image.GetHeight();
	L->rgb.depth    = 8;
	L->rgb.rowBytes = Image.GetWidth() * Image.GetChannelCount();

	result = avifImageYUVToRGB(L->decoder->image, &L->rgb);
	if (result != AVIF_RESULT_OK) {
		throw emException("%s", avifResultToString(result));
	}

	Signal(ChangeSignal);
	return true;
}


void PlAvifImageFileModel::QuitLoading()
{
	if (L) {
		if (L->decoder) avifDecoderDestroy(L->decoder);
		delete L;
		L = NULL;
	}
}


void PlAvifImageFileModel::TryStartSaving()
{
	throw emException("PlAvifImageFileModel: Saving not implemented.");
}


bool PlAvifImageFileModel::TryContinueSaving()
{
	return false;
}


void PlAvifImageFileModel::QuitSaving()
{
}


emUInt64 PlAvifImageFileModel::CalcMemoryNeed()
{
	return
		(emUInt64)
			L->decoder->image->width *
			L->decoder->image->height *
			(L->decoder->alphaPresent ? 4 : 3);
}


double PlAvifImageFileModel::CalcFileProgress()
{
	return 0.0;
}

extern "C" {
	emPanel * PlAvifFpPluginFunc(
		emPanel::ParentArg parent, const emString & name,
		const emString & path, emFpPlugin * plugin,
		emString * errorBuf
	)
	{
		if (plugin->Properties.GetCount()) {
			*errorBuf="PlAvifFpPlugin: No properties allowed.";
			return NULL;
		}
		return new emImageFilePanel(
			parent, name,
			PlAvifImageFileModel::Acquire(
				parent.GetRootContext(), path
			)
		);
	}
}
