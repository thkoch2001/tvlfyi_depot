#include <emCore/emFpPlugin.h>
#include <emCore/emImageFile.h>

/*
QOI Utilities

Copyright (c) 2021, Dominic Szablewski - https://phoboslab.org
SPDX-License-Identifier: MIT
*/

#define QOI_OP_INDEX  0x00 /* 00xxxxxx */
#define QOI_OP_DIFF   0x40 /* 01xxxxxx */
#define QOI_OP_LUMA   0x80 /* 10xxxxxx */
#define QOI_OP_RUN    0xc0 /* 11xxxxxx */
#define QOI_OP_RGB    0xfe /* 11111110 */
#define QOI_OP_RGBA   0xff /* 11111111 */

#define QOI_MASK_2    0xc0 /* 11000000 */

#define QOI_COLOR_HASH(C) (C.GetRed()*3 + C.GetGreen()*5 + C.GetBlue()*7 + C.GetAlpha()*11)

#define QOI_MAGIC \
	(((unsigned int)'q') << 24 | ((unsigned int)'o') << 16 | \
	 ((unsigned int)'i') <<  8 | ((unsigned int)'f'))

#define QOI_HEADER_SIZE 14

static unsigned int qoi_read_32(const unsigned char *bytes, int *p) {
	unsigned int a = bytes[(*p)++];
	unsigned int b = bytes[(*p)++];
	unsigned int c = bytes[(*p)++];
	unsigned int d = bytes[(*p)++];
	return a << 24 | b << 16 | c << 8 | d;
}


class PlQoiImageFileModel : public emImageFileModel
{
public:

	static emRef<PlQoiImageFileModel> Acquire(
		emContext & context, const emString & name, bool common=true
	);

protected:
	PlQoiImageFileModel(emContext & context, const emString & name);
	virtual ~PlQoiImageFileModel();
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


struct PlQoiImageFileModel::LoadingState {
	FILE * file;
	unsigned int width, height, channels;
	size_t file_len;
};


emRef<PlQoiImageFileModel> PlQoiImageFileModel::Acquire(
	emContext & context, const emString & name, bool common
)
{
	EM_IMPL_ACQUIRE(PlQoiImageFileModel, context, name, common)
}


PlQoiImageFileModel::PlQoiImageFileModel(
	emContext & context, const emString & name
)
	: emImageFileModel(context, name)
{
}


PlQoiImageFileModel::~PlQoiImageFileModel()
{
	PlQoiImageFileModel::QuitLoading();
	PlQoiImageFileModel::QuitSaving();
}


void PlQoiImageFileModel::TryStartLoading()
{
	unsigned char header[QOI_HEADER_SIZE];
	unsigned int header_magic, colorspace;
	int pos = 0;

	L = new LoadingState;
	memset(L, 0, sizeof(LoadingState));
	L->file = fopen(GetFilePath(),"rb");
	if (!L->file) throw emException("%s",emGetErrorText(errno).Get());

	if (fread(header, 1, sizeof(header), L->file) != sizeof(header)) {
			if (ferror(L->file)) {
				throw emException("%s",emGetErrorText(errno).Get());
			}
			else  {
				throw emException("QOI header not found");
			}
	}

	header_magic = qoi_read_32(header, &pos);
	L->width = qoi_read_32(header, &pos);
	L->height = qoi_read_32(header, &pos);
	L->channels = header[pos++];
	colorspace = header[pos++];

	if (
		L->width == 0 || L->height == 0 ||
		L->channels < 3 || L->channels > 4 ||
		colorspace > 1 ||
		header_magic != QOI_MAGIC
	) {
		throw emException("QOI header not valid");
	}

	fseek(L->file, 0, SEEK_END);
	L->file_len = ftell(L->file);

	if (L->file_len <= QOI_HEADER_SIZE || fseek(L->file, 0, SEEK_SET) != 0) {
		throw emException("QOI data incomplete");
	}

	FileFormatInfo = "QOI ";
	FileFormatInfo += (
		colorspace ? "all channels linear" : "sRGB with linear alpha"
	);

	Signal(ChangeSignal);
}


bool PlQoiImageFileModel::TryContinueLoading()
{
	emArray<unsigned char> data;
	emColor index[64];
	emColor px { 0, 0, 0, 255 };
	int pos = QOI_HEADER_SIZE;
	int run = 0;

	if (!Image.GetHeight()) {
		Image.Setup(L->width, L->height, L->channels);
	}

	data.SetCount(L->file_len);
	if (fread(data.GetWritable(), 1, L->file_len, L->file) < L->file_len) {
		if (ferror(L->file)) {
			throw emException("%s",emGetErrorText(errno).Get());
		}
		else  {
			throw emException("QOI data incomplete");
		}
	}

	memset(index, 0, sizeof(index));

	for (int px_y = 0; px_y < L->height; px_y++) {
		for (int px_x = 0; px_x < L->width; px_x++) {
			if (run > 0) {
				run--;
			} else if (pos < data.GetCount()) {
				int b1 = data.Get(pos++);

				if (b1 == QOI_OP_RGB) {
					px.SetRed(   data.Get(pos++));
					px.SetGreen( data.Get(pos++));
					px.SetBlue(  data.Get(pos++));
				} else if (b1 == QOI_OP_RGBA) {
					px.SetRed(   data.Get(pos++));
					px.SetGreen( data.Get(pos++));
					px.SetBlue(  data.Get(pos++));
					px.SetAlpha( data.Get(pos++));
				} else if ((b1 & QOI_MASK_2) == QOI_OP_INDEX) {
					px = index[b1];
				} else if ((b1 & QOI_MASK_2) == QOI_OP_DIFF) {
					px.SetRed(
						  px.GetRed() + ((b1 >> 4) & 0x03) - 2);
					px.SetGreen(
						px.GetGreen() + ((b1 >> 2) & 0x03) - 2);
					px.SetBlue(
						 px.GetBlue() + ( b1       & 0x03) - 2);
				} else if ((b1 & QOI_MASK_2) == QOI_OP_LUMA) {
					int b2 = data.Get(pos++);
					int vg = (b1 & 0x3f) - 32;
					px.SetRed(
						  px.GetRed() + vg - 8 + ((b2 >> 4) & 0x0f));
					px.SetGreen(
						px.GetGreen() + vg);
					px.SetBlue(
						 px.GetBlue() + vg - 8 + (b2 & 0x0f));
				} else if ((b1 & QOI_MASK_2) == QOI_OP_RUN) {
					run = (b1 & 0x3f);
				}
				index[QOI_COLOR_HASH(px) % 64] = px;
			}
			Image.SetPixel(px_x, px_y, px);
		}
	}

	Signal(ChangeSignal);
	return true;
}


void PlQoiImageFileModel::QuitLoading()
{
	if (L) {
		if (L->file) fclose(L->file);
		delete L;
		L = NULL;
	}
}


void PlQoiImageFileModel::TryStartSaving()
{
	throw emException("PlQoiImageFileModel: Saving not implemented.");
}


bool PlQoiImageFileModel::TryContinueSaving()
{
	return false;
}


void PlQoiImageFileModel::QuitSaving()
{
}


emUInt64 PlQoiImageFileModel::CalcMemoryNeed()
{
	return
		(emUInt64)L->width * L->height * L->channels + L->file_len;
}


double PlQoiImageFileModel::CalcFileProgress()
{
	return 0.0;
}

extern "C" {
	emPanel * PlQoiFpPluginFunc(
		emPanel::ParentArg parent, const emString & name,
		const emString & path, emFpPlugin * plugin,
		emString * errorBuf
	)
	{
		if (plugin->Properties.GetCount()) {
			*errorBuf="PlQoiFpPlugin: No properties allowed.";
			return NULL;
		}
		return new emImageFilePanel(
			parent, name,
			PlQoiImageFileModel::Acquire(
				parent.GetRootContext(), path
			)
		);
	}
}
