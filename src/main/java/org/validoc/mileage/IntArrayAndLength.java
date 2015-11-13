package org.validoc.mileage;

import java.util.Arrays;

public class IntArrayAndLength {
	public int[] data;
	private int locationSize;

	public IntArrayAndLength(int locationSize, int initialValue) {
		this.data = new int[locationSize * locationSize];
		Arrays.fill(data, initialValue);
		this.locationSize = locationSize;
	}

	public void put(int x, int y, int value) {
		data[x * locationSize + y] = value;
	}

	public int get(int x, int y) {
		return data[x * locationSize + y];
	}

}