package org.validoc.mileage;

import java.util.Arrays;

public class ShortArrayAndLength {
	public short[] data;
	private int locationSize;

	public ShortArrayAndLength(int locationSize, short initialValue) {
		this.data = new short[locationSize * locationSize];
		Arrays.fill(data, initialValue);
		this.locationSize = locationSize;
	}

	public void put(int x, int y, short value) {
		data[x * locationSize + y] = value;
	}

	public short get(int x, int y) {
		return data[x * locationSize + y];
	}

}